{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Chap5.Table where

import Chap5.Symbol
import Chap6.Translate
import Control.Monad.Reader
import Data.IORef

import qualified Data.IntMap as IM

type Table a = IM.IntMap a

empty :: Table a
empty = IM.empty

enter :: Symbol -> a -> Table a -> Table a
enter = IM.insert . symbolValue

look :: Symbol -> Table a -> Maybe a
look = IM.lookup . symbolValue

data Unique = Unique (IORef ())
            | UniqueIgnore
instance Eq Unique where
  UniqueIgnore == _          = True
  _ == UniqueIgnore          = True
  Unique ref1 == Unique ref2 = ref1 == ref2
instance Show Unique where
  show _ = ""

mkUnique :: MonadIO m => m Unique
mkUnique = liftIO $ Unique <$> newIORef ()

newtype TRef = TRef (IORef (Maybe Ty)) deriving (Eq)
instance Show TRef where
  show _ = ""

mkTRef :: MonadIO m => m TRef
mkTRef = liftIO $ TRef <$> newIORef Nothing

readTRef :: MonadIO m => TRef -> m (Maybe Ty)
readTRef (TRef ref) = liftIO $ readIORef ref

writeTRef :: MonadIO m => TRef -> Ty -> m ()
writeTRef (TRef ref) ty = liftIO $ writeIORef ref $ Just ty

data Ty = TInt
        | TString
        | TRecord [(Symbol, Ty)] Unique
        | TArray Ty Unique
        | TNil
        | TUnit
        | TName Symbol TRef
        deriving (Show, Eq)

data EnvEntry frame access = VarEntry (Access frame access) Ty
                           | FunEntry (Level frame) Symbol [Ty] Ty

type TEnv = Table Ty
type VEnv frame access = Table (EnvEntry frame access)

mkEnvs :: forall frame access m
        . MonadIO m
       => SymbolM m
       -> TransM frame access m
       -> m (TEnv, VEnv frame access)
mkEnvs symM transM = (,) <$> convertBase tenvBase <*> (venvBase >>= convertBase)
 where
  tenvBase = [("int", TInt), ("string", TString)]
  venvBase = mapM toFunTuple fns
  fns = [ ("print", [TString], TUnit)
        , ("flush", [], TUnit)
        , ("getchar", [], TString)
        , ("ord", [TString], TInt)
        , ("chr", [TInt], TString)
        , ("size", [TString], TInt)
        , ("substring", [TString, TInt, TInt], TString)
        , ("not", [TInt], TInt)
        , ("exit", [TInt], TUnit) ]

  toFunTuple t@(name, _, _) = (name,) <$> toFunEntry t
  toFunEntry (name, params, ret) = do
    label <- toSymbol symM name
    level <- newLevel transM $ Init Outermost label (False <$ params)
    return $ FunEntry level label params ret
  convertBase :: [(String, s)] -> m (IM.IntMap s)
  convertBase = fmap (IM.fromList . fmap (\(sym, ty) -> (symbolValue sym, ty)))
              . mapM (\(s, ty) -> (, ty) <$> toSymbol symM s)
