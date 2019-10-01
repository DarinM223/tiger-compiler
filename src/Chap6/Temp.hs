module Chap6.Temp where

import Chap5.Symbol
import Control.Monad.Reader
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

mkTempM :: MonadIO m => SymbolM m -> TempRef -> TempM m
mkTempM symM ref = TempM
  { newTemp    = newTemp' ref
  , newLabel   = newLabel' symM ref
  , namedLabel = toSymbol symM
  }

newTemp' :: MonadIO m => TempRef -> m Temp
newTemp' (TempRef ref) = liftIO $ do
  tempData <- readIORef ref
  writeIORef ref tempData { _temp = _temp tempData + 1 }
  return $ _temp tempData

newLabel' :: MonadIO m => SymbolM m -> TempRef -> m Label
newLabel' SymbolM{..} (TempRef ref) = do
  l <- liftIO $ do
    tempData <- readIORef ref
    writeIORef ref tempData { _label = _label tempData + 1 }
    return $ _label tempData
  toSymbol $ "L" ++ show l
