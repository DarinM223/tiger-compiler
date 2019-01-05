module Chap6.Frame where

import qualified Chap6.Temp as Temp

data Init = Init
  { _name    :: Temp.Label
  , _formals :: [Bool]
  }

data FrameM access frame m = FrameM
  { newFrame        :: Init -> m frame
  , allocLocalFrame :: frame -> Bool -> m access
  , name            :: frame -> Temp.Label
  , formals         :: frame -> [access]
  }
