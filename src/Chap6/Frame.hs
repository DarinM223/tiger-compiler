module Chap6.Frame where

import qualified Chap6.Temp as Temp
import qualified Chap7.Tree as Tree

data Init = Init
  { _name    :: Temp.Label
  , _formals :: [Bool]
  }

type WordSize = Int

data FrameM access frame m = FrameM
  { newFrame   :: Init -> m frame
  , allocLocal :: frame -> Bool -> m access
  , name       :: frame -> Temp.Label
  , formals    :: frame -> [access]
  , fp         :: Temp.Temp
  , wordSize   :: WordSize
  , exp        :: access -> Tree.Exp -> Tree.Exp
  }
