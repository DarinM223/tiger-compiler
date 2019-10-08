{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Chap5.Symbol where

import Chap2.Ref
import Text.Megaparsec (SourcePos)

import qualified Data.HashMap.Strict as HM
import qualified Data.HashTable.IO as H

data SymbolTableM k v m = SymbolTableM
  { insertTable :: k -> v -> m ()
  , lookupTable :: k -> m (Maybe v)
  , tableToList :: m [(k, v)]
  }

mkSymbolTableM :: IO (SymbolTableM String Int IO)
mkSymbolTableM = do
  table <- H.new :: IO (H.BasicHashTable k v)
  return SymbolTableM
    { insertTable = H.insert table
    , lookupTable = H.lookup table
    , tableToList = H.toList table
    }

type Pos = SourcePos
newtype Symbol = Symbol { unSymbol :: (String, Int) } deriving (Show)
instance Eq Symbol where
  Symbol (_, i1) == Symbol (_, i2) = i1 == i2

newtype SymbolRef r = SymbolRef { unSymbolRef :: r Int }

mkSymbolRef :: Functor f => RefM r f -> f (SymbolRef r)
mkSymbolRef refM = SymbolRef <$> newRef refM 0

data SymbolM m = SymbolM
  { toSymbol   :: String -> m Symbol
  , getSymbols :: m (HM.HashMap String Int)
  }

mkSymbolM :: Monad m
          => RefM (Ref m) m
          -> SymbolTableM String Int m
          -> SymbolRef (Ref m)
          -> SymbolM m
mkSymbolM RefM{..} SymbolTableM{..} (SymbolRef ref) = SymbolM
  { toSymbol   = toSymbol'
  , getSymbols = getSymbols'
  }
 where
  toSymbol' str = lookupTable str >>= \case
    Just sym -> return $ Symbol (str, sym)
    Nothing  -> do
      sym <- readRef ref
      insertTable str sym
      writeRef ref (sym + 1)
      return $ Symbol (str, sym)
  getSymbols' = HM.fromList <$> tableToList

fromSymbol :: Symbol -> String
fromSymbol = fst . unSymbol

symbolValue :: Symbol -> Int
symbolValue = snd . unSymbol
