module Chap6.Frame where

import qualified Chap6.Temp as Temp

data Init = Init
  { _name    :: Temp.Label
  , _formals :: [Bool]
  }

class Frame access frame | frame -> access where
  name    :: frame -> Temp.Label
  formals :: frame -> [access]

data Access = InFrame Int | InRegister Int
  deriving (Show, Eq)

class (Frame access frame, Monad m) =>
  MonadFrame access frame m | m -> frame access where

  newFrame        :: Init -> m frame
  allocLocalFrame :: frame -> Bool -> m access
