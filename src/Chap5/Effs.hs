{-# LANGUAGE TemplateHaskell #-}
module Chap5.Effs where

import Chap5.Symbol (SymbolM)
import Chap6.Temp (TempM)
import Chap6.Translate (TransM)
import Control.Lens

data SemantEffs frame access m = SemantEffs
  { semantEffsSymbolM :: SymbolM m
  , semantEffsTransM  :: TransM frame access m
  , semantEffsTempM   :: TempM m
  }
makeFields ''SemantEffs
