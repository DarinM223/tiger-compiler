module Chap5.Table where

import Chap5.Symbol
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

mkUnique :: IO Unique
mkUnique = Unique <$> newIORef ()

newtype TRef = TRef (IORef (Maybe Ty)) deriving (Eq)
instance Show TRef where
  show _ = ""

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

mkEnvs :: (MonadIO m, MonadReader r m, HasSymbolRef r, HasSymbolTable r)
       => m (TEnv, VEnv)
mkEnvs = (,) <$> convertBase tenvBase <*> convertBase venvBase
 where
  tenvBase = [("int", TInt), ("string", TString)]
  venvBase = [] -- TODO(DarinM223): add default functions in here.

  convertBase = fmap (IM.fromList . fmap (\(sym, ty) -> (symbolValue sym, ty)))
              . mapM (\(s, ty) -> (, ty) <$> toSymbol s)
