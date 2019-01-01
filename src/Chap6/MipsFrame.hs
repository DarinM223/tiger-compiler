{-# LANGUAGE UndecidableInstances #-}

module Chap6.MipsFrame where

import Control.Monad.Reader
import Chap6.Frame
import qualified Data.Vector as V
import qualified Chap6.Temp as Temp

data Access = InFrame Int | InRegister Int
  deriving (Show, Eq)

data Frame = Frame
  { _name    :: Temp.Label
  , _formals :: [Access]
  , _locals  :: [Access]
  } deriving (Show, Eq)

data MipsData = MipsData
  { _v    :: V.Vector Temp.Temp
  , _a    :: V.Vector Temp.Temp
  , _t    :: V.Vector Temp.Temp
  , _s    :: V.Vector Temp.Temp
  , _zero :: Temp.Temp
  , _gp   :: Temp.Temp
  , _fp   :: Temp.Temp
  , _sp   :: Temp.Temp
  , _ra   :: Temp.Temp
  , _rv   :: Temp.Temp
  }
class HasMipsData r where getMipsData :: r -> MipsData

mkMipsData :: forall r m. (Temp.HasTempRef r, MonadReader r m, MonadIO m)
           => m MipsData
mkMipsData = MipsData
  <$> (V.fromList <$> replicateM 2 Temp.newTemp')
  <*> (V.fromList <$> replicateM 4 Temp.newTemp')
  <*> (V.fromList <$> replicateM 10 Temp.newTemp')
  <*> (V.fromList <$> replicateM 8 Temp.newTemp')
  <*> Temp.newTemp'
  <*> Temp.newTemp'
  <*> Temp.newTemp'
  <*> Temp.newTemp'
  <*> Temp.newTemp'
  <*> Temp.newTemp'

instance FrameOps Access Frame where
  name = _name
  formals = _formals

newtype DeriveFrame m a = DeriveFrame (m a)
  deriving (Functor, Applicative, Monad)
instance (HasMipsData r, MonadReader r m) =>
  MonadFrame Access Frame (DeriveFrame m) where

  newFrame = DeriveFrame . newFrame'
  allocLocalFrame f b = DeriveFrame $ allocLocalFrame' f b

newFrame' :: Init -> m Frame
newFrame' = undefined

allocLocalFrame' :: Frame -> Bool -> m Access
allocLocalFrame' = undefined
