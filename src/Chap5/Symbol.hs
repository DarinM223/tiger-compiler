module Chap5.Symbol where

import Control.Monad.Reader
import Data.IORef
import Text.Megaparsec (SourcePos)

import qualified Data.HashMap.Strict as HM
import qualified Data.HashTable.IO as H

type SymbolTable = H.BasicHashTable String Int
class HasSymbolTable cfg where
  getSymbolTable :: cfg -> SymbolTable

mkSymbolTable :: IO SymbolTable
mkSymbolTable = H.new

type Pos = SourcePos
newtype Symbol = Symbol { unSymbol :: (String, Int) } deriving (Show)
instance Eq Symbol where
  Symbol (_, i1) == Symbol (_, i2) = i1 == i2

newtype SymbolRef = SymbolRef { unSymbolRef :: IORef Int }
class HasSymbolRef cfg where
  getSymbolRef :: cfg -> SymbolRef

mkSymbolRef :: IO SymbolRef
mkSymbolRef = SymbolRef <$> newIORef 0

data SymbolM m = SymbolM
  { toSymbol   :: String -> m Symbol
  , getSymbols :: m (HM.HashMap String Int)
  }
class HasSymbolM m effs | effs -> m where
  getSymbolM :: effs -> SymbolM m

mkSymbolM :: (MonadIO m, HasSymbolRef cfg, HasSymbolTable cfg)
          => cfg -> SymbolM m
mkSymbolM cfg = SymbolM
  { toSymbol   = toSymbol' cfg
  , getSymbols = getSymbols' cfg
  }

toSymbol' :: (MonadIO m, HasSymbolRef cfg, HasSymbolTable cfg)
          => cfg -> String -> m Symbol
toSymbol' cfg str = liftIO (H.lookup table str) >>= \case
  Just sym -> return $ Symbol (str, sym)
  Nothing  -> liftIO $ do
    sym <- readIORef ref
    H.insert table str sym
    writeIORef ref (sym + 1)
    return $ Symbol (str, sym)
 where
  table = getSymbolTable cfg
  SymbolRef ref = getSymbolRef cfg

getSymbols' :: (MonadIO m, HasSymbolTable cfg)
            => cfg -> m (HM.HashMap String Int)
getSymbols' cfg = HM.fromList <$> liftIO (H.toList table)
 where table = getSymbolTable cfg

fromSymbol :: Symbol -> String
fromSymbol = fst . unSymbol

symbolValue :: Symbol -> Int
symbolValue = snd . unSymbol
