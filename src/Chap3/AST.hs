{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
module Chap3.AST where

import Chap5.Symbol

data RefM ref m = RefM
  { newRef   :: forall a. a -> m (ref a)
  , readRef  :: forall a. ref a -> m a
  , writeRef :: forall a. ref a -> a -> m ()
  }

newtype Escape ref = Escape { unEscape :: ref Bool }
instance Show (Escape ref) where
    show _ = ""

mkEscape :: Functor f => (?refM :: RefM r f) => f (Escape r)
mkEscape = Escape <$> newRef ?refM False

readEscape :: (?refM :: RefM r f) => Escape r -> f Bool
readEscape (Escape ref) = readRef ?refM ref

data Var r = SimpleVar Symbol Pos
           | FieldVar (Var r) Symbol Pos
           | SubscriptVar (Var r) (Exp r) Pos
           deriving (Show)

data Exp r = VarExp (Var r)
           | NilExp
           | IntExp Int
           | StringExp Pos String
           | CallExp (CallExp' r)
           | OpExp (Exp r) Op (Exp r) Pos
           | RecordExp (RecordExp' r)
           | SeqExp [(Pos, Exp r)]
           | AssignExp Pos (Var r) (Exp r)
           | IfExp (IfExp' r)
           | WhileExp (WhileExp' r)
           | ForExp (ForExp' r)
           | BreakExp Pos
           | LetExp (LetExp' r)
           | ArrayExp (ArrayExp' r)
           deriving (Show)

data CallExp' r = CallExp'
  { _pos  :: Pos
  , _func :: Symbol
  , _args :: [Exp r]
  } deriving (Show)

data RecordExp' r = RecordExp'
  { _pos    :: Pos
  , _type   :: Symbol
  , _fields :: [(Pos, Symbol, Exp r)]
  } deriving (Show)

data IfExp' r = IfExp'
  { _pos   :: Pos
  , _test  :: Exp r
  , _then' :: Exp r
  , _else' :: Maybe (Exp r)
  } deriving (Show)

data WhileExp' r = WhileExp'
  { _pos  :: Pos
  , _test :: Exp r
  , _body :: Exp r
  } deriving (Show)

data ForExp' r = ForExp'
  { _pos    :: Pos
  , _var    :: Symbol
  , _escape :: Escape r
  , _lo     :: Exp r
  , _hi     :: Exp r
  , _body   :: Exp r
  } deriving (Show)

data LetExp' r = LetExp'
  { _pos  :: Pos
  , _decs :: [Dec r]
  , _body :: Exp r
  } deriving (Show)

data ArrayExp' r = ArrayExp'
  { _pos  :: Pos
  , _type :: Symbol
  , _size :: Exp r
  , _init :: Exp r
  } deriving (Show)

data Dec r = FunctionDec [FunDec r]
           | VarDec (VarDec' r)
           | TypeDec [TypeDec' r]
           deriving (Show)

data Field r = Field
  { _pos    :: Pos
  , _name   :: Symbol
  , _type   :: Symbol
  , _escape :: Escape r
  } deriving (Show)

data FunDec r = FunDec
  { _pos    :: Pos
  , _name   :: Symbol
  , _params :: [Field r]
  , _result :: Maybe (Pos, Symbol)
  , _body   :: Exp r
  } deriving (Show)

data VarDec' r = VarDec'
  { _pos    :: Pos
  , _name   :: Symbol
  , _type   :: Maybe (Pos, Symbol)
  , _init   :: Exp r
  , _escape :: Escape r
  } deriving (Show)

data TypeDec' r = TypeDec'
  { _pos  :: Pos
  , _name :: Symbol
  , _ty   :: Ty r
  } deriving (Show)

data Ty r = NameTy Symbol Pos
          | RecordTy [Field r]
          | ArrayTy Symbol Pos
          deriving (Show)

data Op = PlusOp | MinusOp | TimesOp | DivideOp
        | EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp
        | AndOp | OrOp
        deriving (Show, Eq)
