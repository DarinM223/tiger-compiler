module Chap6.Temp where

import Chap5.Symbol
import Control.Monad.Reader
import Data.Generics.Product.Typed
import Data.IORef

newtype Temp = Temp { unTemp :: Int } deriving (Eq, Num)
instance Show Temp where
  show (Temp t) = "t" ++ show t
type Label = Symbol

data TempM m = TempM
  { newTemp    :: m Temp
  , newLabel   :: m Label
  , namedLabel :: String -> m Label
  }

data TempData = TempData
  { _temp  :: Temp
  , _label :: Int
  }
newtype TempRef = TempRef (IORef TempData)

mkTempRef :: MonadIO m => m TempRef
mkTempRef = liftIO $ TempRef <$> newIORef TempData
  { _temp = 100, _label = 0 }

tempMIO :: (HasType TempRef r, MonadReader r m, MonadIO m)
        => SymbolM m
        -> TempM m
tempMIO symM = TempM
  { newTemp    = newTemp'
  , newLabel   = newLabel' symM
  , namedLabel = toSymbol symM
  }

newTemp' :: (HasType TempRef r, MonadReader r m, MonadIO m) => m Temp
newTemp' = do
  TempRef ref <- asks $ getTyped @TempRef
  liftIO $ do
    tempData <- readIORef ref
    writeIORef ref tempData { _temp = _temp tempData + 1 }
    return $ _temp tempData

newLabel' :: (HasType TempRef r, MonadReader r m, MonadIO m)
          => SymbolM m
          -> m Label
newLabel' symM = do
  TempRef ref <- asks $ getTyped @TempRef
  l <- liftIO $ do
    tempData <- readIORef ref
    writeIORef ref tempData { _label = _label tempData + 1 }
    return $ _label tempData
  toSymbol symM $ "L" ++ show l
