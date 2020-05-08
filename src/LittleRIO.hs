{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Most definitions follow the RIO lib: https://hackage.haskell.org/package/rio-0.1.14.0/docs/RIO.html
See LICENSE info in the README.
-}
module LittleRIO
  ( HasStateRef (..)
  , HasWriteRef (..)
  , SomeRef (..)
  , RIO (..)
  , getStateRef
  , liftRIO
  , listenWriteRef
  , newSomeRef
  , mapRIO
  , modifySomeRef
  , modifyStateRef
  , passWriteRef
  , putStateRef
  , runRIO
  , readSomeRef
  , tellWriteRef
  , unliftRIO
  , writeSomeRef
  ) where

import Control.Applicative (liftA2)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO, UnliftIO, askUnliftIO)
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.State (MonadState (..))
import Control.Monad.Writer (MonadWriter (..))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Lens.Micro (Lens')
import Lens.Micro.Mtl (view)
import Prelude

newtype RIO env a = RIO { unRIO :: ReaderT env IO a }
  deriving (Functor, Applicative, Monad, MonadReader env, MonadIO, MonadThrow, MonadFail, MonadCatch, MonadMask, MonadUnliftIO)

instance Semigroup a => Semigroup (RIO env a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (RIO env a) where
  mempty = pure mempty
  mappend = (<>)

mapRIO :: (env -> env') -> RIO env' a -> RIO env a
mapRIO f m = do
  env <- ask
  let env' = f env
  runRIO env' m

liftRIO :: (MonadIO m, MonadReader env m) => RIO env a -> m a
liftRIO rio = do
  env <- ask
  runRIO env rio

unliftRIO :: MonadIO m => env -> m (UnliftIO (RIO env))
unliftRIO env = liftIO (runRIO env askUnliftIO)

runRIO :: MonadIO m => env -> RIO env a -> m a
runRIO r m = liftIO (runReaderT (unRIO m) r)

data SomeRef a = SomeRef !(IO a) !(a -> IO ())

readSomeRef :: MonadIO m => SomeRef a -> m a
readSomeRef (SomeRef x _) = liftIO x

writeSomeRef :: MonadIO m => SomeRef a -> a -> m ()
writeSomeRef (SomeRef _ x) = liftIO . x

modifySomeRef :: MonadIO m => SomeRef a -> (a -> a) -> m ()
modifySomeRef (SomeRef read' write) f = liftIO (fmap f read' >>= write)

ioRefToSomeRef :: IORef a -> SomeRef a
ioRefToSomeRef ref = SomeRef (readIORef ref) (writeIORef ref)

newSomeRef :: MonadIO m => a -> m (SomeRef a)
newSomeRef = liftIO . fmap ioRefToSomeRef . newIORef

class HasStateRef st env | env -> st where
  stateRefL :: Lens' env (SomeRef st)

instance HasStateRef a (SomeRef a) where
  stateRefL = id

getStateRef :: (HasStateRef st env, MonadReader env m, MonadIO m) => m st
getStateRef = do
  ref <- view stateRefL
  liftIO (readSomeRef ref)

putStateRef :: (HasStateRef st env, MonadReader env m, MonadIO m) => st -> m ()
putStateRef st = do
  ref <- view stateRefL
  liftIO (writeSomeRef ref st)

modifyStateRef :: (HasStateRef st env, MonadReader env m, MonadIO m) => (st -> st) -> m ()
modifyStateRef f = do
  ref <- view stateRefL
  liftIO (modifySomeRef ref f)

instance HasStateRef st env => MonadState st (RIO env) where
  get = getStateRef
  put = putStateRef

class HasWriteRef w env | env -> w where
  writeRefL :: Lens' env (SomeRef w)

instance HasWriteRef a (SomeRef a) where
  writeRefL = id

tellWriteRef :: (HasWriteRef w env, MonadReader env m, MonadIO m, Semigroup w) => w -> m ()
tellWriteRef value = do
  ref <- view writeRefL
  liftIO $ modifySomeRef ref (<> value)

listenWriteRef :: (HasWriteRef w env, MonadReader env m, MonadIO m) => m a -> m (a, w)
listenWriteRef action = do
  w1 <- view writeRefL >>= liftIO . readSomeRef
  a <- action
  w2 <- do
    refEnv <- view writeRefL
    v <- liftIO $ readSomeRef refEnv
    _ <- liftIO $ writeSomeRef refEnv w1
    return v
  return (a, w2)

passWriteRef :: (HasWriteRef w env, MonadReader env m, MonadIO m) => m (a, w -> w) -> m a
passWriteRef action = do
  (a, transF) <- action
  ref <- view writeRefL
  liftIO $ modifySomeRef ref transF
  return a

instance (Monoid w, HasWriteRef w env) => MonadWriter w (RIO env) where
  tell = tellWriteRef
  listen = listenWriteRef
  pass = passWriteRef
