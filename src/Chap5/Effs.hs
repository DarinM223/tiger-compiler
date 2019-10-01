{-# LANGUAGE TemplateHaskell #-}
module Chap5.Effs where

import Chap5.Symbol (HasSymbolM (..), SymbolM)
import Chap6.Temp (HasTempM (..), TempM)
import Chap6.Translate (HasTransM (..), TransM)
import Control.Lens (makeFields)

data SemantEffs frame access m = SemantEffs
  { semantEffsSymbolM :: SymbolM m
  , semantEffsTransM  :: TransM frame access m
  , semantEffsTempM   :: TempM m
  }
makeFields ''SemantEffs
