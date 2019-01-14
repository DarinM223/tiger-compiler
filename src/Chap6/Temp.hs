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
class HasTempM m effs | effs -> m where
  getTempM :: effs -> TempM m

data TempData = TempData
  { _temp  :: Temp
  , _label :: Int
  }

newtype TempRef = TempRef (IORef TempData)
class HasTempRef cfg where
  getTempRef :: cfg -> TempRef

mkTempRef :: MonadIO m => m TempRef
mkTempRef = liftIO $ TempRef <$> newIORef TempData
  { _temp = 100, _label = 0 }

mkTempM :: (HasTempRef cfg, MonadIO m) => SymbolM m -> cfg -> TempM m
mkTempM symM cfg = TempM
  { newTemp    = newTemp' cfg
  , newLabel   = newLabel' symM cfg
  , namedLabel = toSymbol symM
  }

newTemp' :: (HasTempRef cfg, MonadIO m) => cfg -> m Temp
newTemp' cfg = liftIO $ do
  tempData <- readIORef ref
  writeIORef ref tempData { _temp = _temp tempData + 1 }
  return $ _temp tempData
 where TempRef ref = getTempRef cfg

newLabel' :: (HasTempRef cfg, MonadIO m) => SymbolM m -> cfg -> m Label
newLabel' symM cfg = do
  l <- liftIO $ do
    tempData <- readIORef ref
    writeIORef ref tempData { _label = _label tempData + 1 }
    return $ _label tempData
  toSymbol symM $ "L" ++ show l
 where TempRef ref = getTempRef cfg
