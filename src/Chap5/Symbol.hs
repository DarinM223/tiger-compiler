module Chap5.Symbol where

import Control.Lens (Lens')
import Control.Monad.Reader
import Data.IORef
import Text.Megaparsec (SourcePos)

import qualified Data.HashMap.Strict as HM
import qualified Data.HashTable.IO as H

type SymbolTable = H.BasicHashTable String Int

mkSymbolTable :: IO SymbolTable
mkSymbolTable = H.new

type Pos = SourcePos
newtype Symbol = Symbol { unSymbol :: (String, Int) } deriving (Show)
instance Eq Symbol where
  Symbol (_, i1) == Symbol (_, i2) = i1 == i2

newtype SymbolRef = SymbolRef { unSymbolRef :: IORef Int }

mkSymbolRef :: IO SymbolRef
mkSymbolRef = SymbolRef <$> newIORef 0

data SymbolM m = SymbolM
  { toSymbol   :: String -> m Symbol
  , getSymbols :: m (HM.HashMap String Int)
  }

class HasSymbolM s a | s -> a where
  symbolM :: Lens' s a

mkSymbolM :: MonadIO m => SymbolRef -> SymbolTable -> SymbolM m
mkSymbolM ref table = SymbolM
  { toSymbol   = toSymbol' ref table
  , getSymbols = getSymbols' table
  }

toSymbol' :: MonadIO m => SymbolRef -> SymbolTable -> String -> m Symbol
toSymbol' (SymbolRef ref) table str = liftIO (H.lookup table str) >>= \case
  Just sym -> return $ Symbol (str, sym)
  Nothing  -> liftIO $ do
    sym <- readIORef ref
    H.insert table str sym
    writeIORef ref (sym + 1)
    return $ Symbol (str, sym)

getSymbols' :: MonadIO m => SymbolTable -> m (HM.HashMap String Int)
getSymbols' table = HM.fromList <$> liftIO (H.toList table)

fromSymbol :: Symbol -> String
fromSymbol = fst . unSymbol

symbolValue :: Symbol -> Int
symbolValue = snd . unSymbol
