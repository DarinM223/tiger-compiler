{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
module Chap2.Ref where

import Data.Functor.Classes (Eq1 (liftEq))
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.IORef as IORef

newtype IORef a = IORef { unIORef :: IORef.IORef a } deriving Eq

newIORef :: a -> IO (IORef a)
newIORef v = IORef <$> IORef.newIORef v

readIORef :: IORef a -> IO a
readIORef (IORef ref) = IORef.readIORef ref

writeIORef :: IORef a -> a -> IO ()
writeIORef (IORef ref) = IORef.writeIORef ref

instance Eq1 IORef where
  -- NOTE(DarinM223): uses unsafeCoerce here because
  -- Eq for IORef only checks for pointer equality
  -- so you can compare IORef a and IORef b even though
  -- they are different types.
  liftEq _ f1 f2 = f1 == unsafeCoerce f2

type family Ref (m :: * -> *) :: * -> *
type instance Ref IO = IORef

data RefM ref m = RefM
  { newRef   :: forall a. a -> m (ref a)
  , readRef  :: forall a. ref a -> m a
  , writeRef :: forall a. ref a -> a -> m ()
  }
