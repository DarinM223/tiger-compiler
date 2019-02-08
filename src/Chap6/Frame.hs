module Chap6.Frame where

import qualified Chap6.Temp as Temp

data Init = Init
  { _name    :: Temp.Label
  , _formals :: [Bool]
  }

type WordSize = Int

data FrameM access frame exp m = FrameM
  { newFrame   :: Init -> m frame
  , allocLocal :: frame -> Bool -> m access
  , name       :: frame -> Temp.Label
  , formals    :: frame -> [access]
  , fp         :: Temp.Temp
  , wordSize   :: WordSize
  , frameExp   :: access -> exp -> exp
  }
