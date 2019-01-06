module Chap5.Table where

import Chap5.Symbol
import Control.Monad.Reader
import Data.Generics.Product.Typed
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

mkUnique :: (MonadIO m) => m Unique
mkUnique = liftIO $ Unique <$> newIORef ()

newtype TRef = TRef (IORef (Maybe Ty)) deriving (Eq)
instance Show TRef where
  show _ = ""

mkTRef :: (MonadIO m) => m TRef
mkTRef = liftIO $ TRef <$> newIORef Nothing

readTRef :: (MonadIO m) => TRef -> m (Maybe Ty)
readTRef (TRef ref) = liftIO $ readIORef ref

writeTRef :: (MonadIO m) => TRef -> Ty -> m ()
writeTRef (TRef ref) ty = liftIO $ writeIORef ref $ Just ty

data Ty = TInt
        | TString
        | TRecord [(Symbol, Ty)] Unique
        | TArray Ty Unique
        | TNil
        | TUnit
        | TName Symbol TRef
        deriving (Show, Eq)

data EnvEntry = VarEntry Ty | FunEntry [Ty] Ty

type TEnv = Table Ty
type VEnv = Table EnvEntry

mkEnvs :: ( MonadIO m, MonadReader r m
          , HasType SymbolRef r, HasType SymbolTable r )
       => m (TEnv, VEnv)
mkEnvs = (,) <$> convertBase tenvBase <*> convertBase venvBase
 where
  tenvBase = [("int", TInt), ("string", TString)]
  venvBase =
    [ ("print", FunEntry [TString] TUnit)
    , ("flush", FunEntry [] TUnit)
    , ("getchar", FunEntry [] TString)
    , ("ord", FunEntry [TString] TInt)
    , ("chr", FunEntry [TInt] TString)
    , ("size", FunEntry [TString] TInt)
    , ("substring", FunEntry [TString, TInt, TInt] TString)
    , ("not", FunEntry [TInt] TInt)
    , ("exit", FunEntry [TInt] TUnit)
    ]

  convertBase = fmap (IM.fromList . fmap (\(sym, ty) -> (symbolValue sym, ty)))
              . mapM (\(s, ty) -> (, ty) <$> toSymbol s)
