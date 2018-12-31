module Chap6.MipsFrame where

import Chap6.Frame
import qualified Chap6.Temp as Temp

data Access = InFrame Int | InRegister Int
  deriving (Show, Eq)

data Frame = Frame
  { _name    :: Temp.Label
  , _formals :: [Access]
  , _locals  :: [Access]
  } deriving (Show, Eq)

instance FrameOps Access Frame where
  name = _name
  formals = _formals

newtype DeriveFrame m a = DeriveFrame (m a)
instance (Monad m) => MonadFrame Access Frame m where
  newFrame = undefined
  allocLocalFrame = undefined
