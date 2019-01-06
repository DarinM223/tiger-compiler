module Chap6.MipsFrame where

import Control.Monad.Catch
import Control.Monad.Reader
import Chap6.Frame
import Chap6.Temp (TempRef, TempM (..))
import Data.Foldable
import Data.Generics.Product.Typed
import Data.IORef
import GHC.Records
import qualified Data.Vector as V
import qualified Chap6.Temp as Temp

data FrameException = TooManyArgs Int deriving (Show)
instance Exception FrameException

data Access = InFrame Int | InRegister Int
  deriving (Show, Eq)

data Frame = Frame
  { _name    :: Temp.Label
  , _formals :: [Access]
  , _locals  :: IORef Int
  }

data MipsData = MipsData
  { _v    :: V.Vector Temp.Temp
  , _a    :: V.Vector Temp.Temp
  , _t    :: V.Vector Temp.Temp
  , _s    :: V.Vector Temp.Temp
  , _zero :: Temp.Temp
  , _gp   :: Temp.Temp
  , _fp   :: Temp.Temp
  , _sp   :: Temp.Temp
  , _ra   :: Temp.Temp
  , _rv   :: Temp.Temp
  }
class HasMipsData r where getMipsData :: r -> MipsData

mkMipsData :: forall r m. (HasType TempRef r, MonadReader r m, MonadIO m)
           => m MipsData
mkMipsData = MipsData
  <$> (V.fromList <$> replicateM 2 Temp.newTemp')
  <*> (V.fromList <$> replicateM 4 Temp.newTemp')
  <*> (V.fromList <$> replicateM 10 Temp.newTemp')
  <*> (V.fromList <$> replicateM 8 Temp.newTemp')
  <*> Temp.newTemp'
  <*> Temp.newTemp'
  <*> Temp.newTemp'
  <*> Temp.newTemp'
  <*> Temp.newTemp'
  <*> Temp.newTemp'

frameMIO :: (MonadIO m, MonadThrow m) => TempM m -> FrameM Access Frame m
frameMIO tempM = FrameM
  { newFrame        = newFrame' tempM
  , allocLocalFrame = allocLocalFrame' tempM
  , name            = getField @"_name"
  , formals         = getField @"_formals"
  }

wordSize :: Int
wordSize = 4

newFrame' :: (MonadIO m, MonadThrow m) => TempM m -> Init -> m Frame
newFrame' tempM init
  | formalsLen > 4 = throwM $ TooManyArgs formalsLen
  | otherwise      = Frame
    <$> pure (getField @"_name" init)
    <*> allocFormals
    <*> liftIO (newIORef 0)
 where
  formalsLen = length $ getField @"_formals" init
  allocFormals = fmap fst
               . foldrM allocFormal ([], wordSize)
               $ getField @"_formals" init
  allocFormal escapes (formals, offset)
    | escapes   = return (InFrame offset:formals, offset + wordSize)
    | otherwise = do
      access <- InRegister . Temp.unTemp <$> newTemp tempM
      return (access:formals, offset)

allocLocalFrame' :: MonadIO m => TempM m -> Frame -> Bool -> m Access
allocLocalFrame' tempM frame escapes
  | escapes = do
    locals <- liftIO $ readIORef (_locals frame)
    let locals' = locals + 1
        offset  = locals' * (-wordSize)
    liftIO $ writeIORef (_locals frame) locals'
    return $ InFrame offset
  | otherwise = InRegister . Temp.unTemp <$> newTemp tempM
