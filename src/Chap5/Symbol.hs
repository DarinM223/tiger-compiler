module Chap5.Symbol where

import Control.Monad.Reader
import Data.IORef
import Text.Megaparsec (SourcePos)

import qualified Data.HashMap.Strict as HM
import qualified Data.HashTable.IO as H

type SymbolTable = H.BasicHashTable String Int

class HasSymbolTable r where
  getSymTable :: r -> SymbolTable

mkSymbolTable :: IO SymbolTable
mkSymbolTable = H.new

type Pos = SourcePos
newtype Symbol = Symbol { unSymbol :: (String, Int) } deriving (Show)
instance Eq Symbol where
  Symbol (_, i1) == Symbol (_, i2) = i1 == i2

newtype SymbolRef = SymbolRef { unSymbolRef :: IORef Int }
class HasSymbolRef r where
  getSymRef :: r -> SymbolRef
instance HasSymbolRef SymbolRef where
  getSymRef = id

mkSymbolRef :: IO SymbolRef
mkSymbolRef = SymbolRef <$> newIORef 0

toSymbol :: (MonadIO m, MonadReader r m, HasSymbolRef r, HasSymbolTable r)
         => String
         -> m Symbol
toSymbol str = do
  table <- asks getSymTable
  liftIO (H.lookup table str) >>= \case
    Just sym -> return $ Symbol (str, sym)
    Nothing  -> do
      SymbolRef ref <- asks getSymRef
      liftIO $ do
        sym <- readIORef ref
        H.insert table str sym
        writeIORef ref (sym + 1)
        return $ Symbol (str, sym)

getSymbols :: (MonadIO m, MonadReader r m, HasSymbolTable r)
           => m (HM.HashMap String Int)
getSymbols = do
  table <- asks getSymTable
  l <- liftIO $ H.toList table
  return $ HM.fromList l

fromSymbol :: Symbol -> String
fromSymbol = fst . unSymbol

symbolValue :: Symbol -> Int
symbolValue = snd . unSymbol
