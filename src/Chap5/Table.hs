module Chap5.Table where

import Chap5.Symbol
import Data.IORef

import qualified Data.IntMap as IM

type Table a = IM.IntMap a

empty :: Table a
empty = IM.empty

enter :: Symbol -> a -> Table a -> Table a
enter = IM.insert . symbolValue

look :: Symbol -> Table a -> Maybe a
look = IM.lookup . symbolValue

newtype Unique = Unique (IORef ()) deriving (Eq)
instance Show Unique where
  show _ = ""

data Ty = TInt
        | TString
        | TRecord [(Symbol, Ty)] Unique
        | TArray Ty Unique
        | TNil
        | TUnit
        | TName Symbol (IORef (Maybe Ty))

data EnvEntry = VarEntry Ty | FunEntry [Ty] Ty

type TEnv = Table Ty
type VEnv = Table EnvEntry
