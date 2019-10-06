{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
module Chap6.Temp where

import Chap3.AST
import Chap5.Symbol

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

newtype TempRef r = TempRef (r TempData)

mkTempRef :: Functor m => RefM r m -> m (TempRef r)
mkTempRef refM = TempRef <$> newRef refM TempData
  { _temp = 100, _label = 0 }

mkTempM :: Monad m => RefM r m -> SymbolM m -> TempRef r -> TempM m
mkTempM refM symM ref = TempM
  { newTemp    = newTemp' refM ref
  , newLabel   = newLabel' refM symM ref
  , namedLabel = toSymbol symM
  }

newTemp' :: Monad m => RefM r m -> TempRef r -> m Temp
newTemp' RefM{..} (TempRef ref) = do
  tempData <- readRef ref
  writeRef ref tempData { _temp = _temp tempData + 1 }
  return $ _temp tempData

newLabel' :: Monad m => RefM r m -> SymbolM m -> TempRef r -> m Label
newLabel' RefM{..} SymbolM{..} (TempRef ref) = do
  l <- do
    tempData <- readRef ref
    writeRef ref tempData { _label = _label tempData + 1 }
    return $ _label tempData
  toSymbol $ "L" ++ show l
