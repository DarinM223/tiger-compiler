module Chap5.Symbol where

import Control.Monad.Reader
import Data.Generics.Product.Typed
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

symbolMIO :: ( MonadIO m, MonadReader r m
             , HasType SymbolRef r, HasType SymbolTable r )
          => SymbolM m
symbolMIO = SymbolM
  { toSymbol   = toSymbol'
  , getSymbols = getSymbols'
  }

toSymbol' :: ( MonadIO m, MonadReader r m
             , HasType SymbolRef r, HasType SymbolTable r )
          => String
          -> m Symbol
toSymbol' str = do
  table <- asks $ getTyped @SymbolTable
  liftIO (H.lookup table str) >>= \case
    Just sym -> return $ Symbol (str, sym)
    Nothing  -> do
      SymbolRef ref <- asks $ getTyped @SymbolRef
      liftIO $ do
        sym <- readIORef ref
        H.insert table str sym
        writeIORef ref (sym + 1)
        return $ Symbol (str, sym)

getSymbols' :: (MonadIO m, MonadReader r m, HasType SymbolTable r)
            => m (HM.HashMap String Int)
getSymbols' = do
  table <- asks $ getTyped @SymbolTable
  l <- liftIO $ H.toList table
  return $ HM.fromList l

fromSymbol :: Symbol -> String
fromSymbol = fst . unSymbol

symbolValue :: Symbol -> Int
symbolValue = snd . unSymbol
