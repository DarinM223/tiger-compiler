{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Chap5.Table where

import Chap3.AST (RefM (..))
import Chap5.Symbol
import Chap6.Translate
import Data.Functor.Classes (Eq1, eq1)

import qualified Data.IntMap as IM

type Table a = IM.IntMap a

empty :: Table a
empty = IM.empty

enter :: Symbol -> a -> Table a -> Table a
enter = IM.insert . symbolValue

look :: Symbol -> Table a -> Maybe a
look = IM.lookup . symbolValue

data Unique r = Unique (r ()) | UniqueIgnore
instance Eq1 r => Eq (Unique r) where
  UniqueIgnore == _          = True
  _ == UniqueIgnore          = True
  Unique ref1 == Unique ref2 = eq1 ref1 ref2
instance Show (Unique r) where
  show _ = ""

mkUnique :: Functor m => (?refM :: RefM r m) => m (Unique r)
mkUnique = Unique <$> newRef ?refM ()

newtype TRef r = TRef (r (Maybe (Ty r)))
instance Eq1 r => Eq (TRef r) where
  TRef r1 == TRef r2 = eq1 r1 r2
instance Show (TRef r) where
  show _ = ""

mkTRef :: Functor m => (?refM :: RefM r m) => m (TRef r)
mkTRef = TRef <$> newRef ?refM Nothing

readTRef :: (?refM :: RefM r m) => TRef r -> m (Maybe (Ty r))
readTRef (TRef ref) =  readRef ?refM ref

writeTRef :: (?refM :: RefM r m) => TRef r -> Ty r -> m ()
writeTRef (TRef ref) ty = writeRef ?refM ref $ Just ty

data Ty r = TInt
          | TString
          | TRecord [(Symbol, Ty r)] (Unique r)
          | TArray (Ty r) (Unique r)
          | TNil
          | TUnit
          | TName Symbol (TRef r)
          deriving (Show, Eq)

data EnvEntry ref frame access
  = VarEntry (Access frame access) (Ty ref)
  | FunEntry (Level frame) Symbol [Ty ref] (Ty ref)

type TEnv ref = Table (Ty ref)
type VEnv ref frame access = Table (EnvEntry ref frame access)

mkEnvs :: forall r frame access m
        . Monad m
       => SymbolM m
       -> TransM frame access m
       -> m (TEnv r, VEnv r frame access)
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
