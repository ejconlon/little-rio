{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Most definitions follow the RIO lib: https://hackage.haskell.org/package/rio-0.1.18.0/docs/RIO.html
The rest follow from orphans: https://hackage.haskell.org/package/rio-orphans-0.1.1.0/docs/src/RIO.Orphans.html
See LICENSE info in the README.
-}
module LittleRIO
  ( HasResourceMap (..)
  , HasStateRef (..)
  , HasWriteRef (..)
  , SimpleResourceEnv (..)
  , SimpleStateEnv (..)
  , SimpleWriteEnv (..)
  , SomeRef (..)
  , ResourceMap
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
  , runSimpleResourceRIO
  , runSimpleStateRIO
  , runSimpleWriteRIO
  , runRIO
  , readSomeRef
  , resourceRIO
  , tellWriteRef
  , unliftRIO
  , withResourceMap
  , writeSomeRef
  ) where

import Control.Applicative (liftA2)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO (..), UnliftIO, askUnliftIO)
import Control.Monad.Primitive (PrimMonad (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.State (MonadState (..))
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Trans.Resource.Internal (MonadResource (..), ReleaseMap, ResourceT (..))
import Control.Monad.Writer (MonadWriter (..))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Lens.Micro (Lens', lens)
import Lens.Micro.Mtl (view)
import Prelude

newtype RIO env a = RIO { unRIO :: ReaderT env IO a }
  deriving (Functor, Applicative, Monad, MonadReader env, MonadIO, MonadThrow, MonadFail, MonadCatch, MonadMask, MonadUnliftIO)

instance Semigroup a => Semigroup (RIO env a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (RIO env a) where
  mempty = pure mempty
  mappend = (<>)

instance PrimMonad (RIO env) where
  type PrimState (RIO env) = PrimState IO
  primitive = RIO . ReaderT . const . primitive

mapRIO :: (env -> env') -> RIO env' a -> RIO env a
mapRIO f m = do
  env <- ask
  let env' = f env
  runRIO env' m

liftRIO :: (MonadIO m, MonadReader env m) => RIO env a -> m a
liftRIO m = do
  env <- ask
  runRIO env m

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
modifySomeRef (SomeRef read' write) f = liftIO (read' >>= write . f)

ioRefToSomeRef :: IORef a -> SomeRef a
ioRefToSomeRef ref = SomeRef (readIORef ref) (writeIORef ref)

newSomeRef :: MonadIO m => a -> m (SomeRef a)
newSomeRef = liftIO . fmap ioRefToSomeRef . newIORef

class HasStateRef st env | env -> st where
  stateRefL :: Lens' env (SomeRef st)

instance HasStateRef a (SomeRef a) where
  stateRefL = id

data SimpleStateEnv st env = SimpleStateEnv
  { sseRef :: !(SomeRef st)
  , sseEnv :: !env
  } deriving (Functor, Foldable, Traversable)

instance HasStateRef st (SimpleStateEnv st env) where
  stateRefL = lens sseRef (\(SimpleStateEnv _ env) st -> SimpleStateEnv st env)

runSimpleStateRIO :: MonadIO m => st -> env -> RIO (SimpleStateEnv st env) a -> m (a, st)
runSimpleStateRIO st env m = do
  ref <- newSomeRef st
  a <- runRIO (SimpleStateEnv ref env) m
  st' <- readSomeRef ref
  pure (a, st')

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

data SimpleWriteEnv w env = SimpleWriteEnv
  { sweRef :: !(SomeRef w)
  , sweEnv :: !env
  } deriving (Functor, Foldable, Traversable)

instance HasWriteRef w (SimpleWriteEnv w env) where
  writeRefL = lens sweRef (\(SimpleWriteEnv _ env) w -> SimpleWriteEnv w env)

runSimpleWriteRIO :: (MonadIO m, Monoid w) => env -> RIO (SimpleWriteEnv w env) a -> m (a, w)
runSimpleWriteRIO env m = do
  ref <- newSomeRef mempty
  a <- runRIO (SimpleWriteEnv ref env) m
  w <- readSomeRef ref
  pure (a, w)

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

type ResourceMap = IORef ReleaseMap

withResourceMap :: MonadUnliftIO m => (ResourceMap -> m a) -> m a
withResourceMap inner =
  withRunInIO (\run -> runResourceT (ResourceT (run . inner)))

class HasResourceMap env where
  resourceMapL :: Lens' env ResourceMap

instance HasResourceMap (IORef ReleaseMap) where
  resourceMapL = id

data SimpleResourceEnv env = SimpleResourceEnv
  { sreMap :: !ResourceMap
  , sreEnv :: !env
  } deriving (Functor, Foldable, Traversable)

instance HasResourceMap (SimpleResourceEnv env) where
  resourceMapL = lens sreMap (\(SimpleResourceEnv _ env) m -> SimpleResourceEnv m env)

runSimpleResourceRIO :: MonadUnliftIO m => env -> RIO (SimpleResourceEnv env) a -> m a
runSimpleResourceRIO env m = withResourceMap (\rm -> runRIO (SimpleResourceEnv rm env) m)

resourceRIO :: HasResourceMap env => ResourceT IO a -> RIO env a
resourceRIO (ResourceT f) = view resourceMapL >>= liftIO . f

instance HasResourceMap env => MonadResource (RIO env) where
  liftResourceT = resourceRIO
