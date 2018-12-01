module Chap3.AST where

import Chap5.Symbol
import Data.IORef

newtype Escape = Escape { unEscape :: IORef Bool }
instance Show Escape where
    show _ = ""

mkEscape :: IO Escape
mkEscape = Escape <$> newIORef False

data Var = SimpleVar Symbol Pos
         | FieldVar Var Symbol Pos
         | SubscriptVar Var Exp Pos
         deriving (Show)

data Exp = VarExp Var
         | NilExp
         | IntExp Int
         | StringExp Pos String
         | CallExp CallExp'
         | OpExp Exp Op Exp Pos
         | RecordExp RecordExp'
         | SeqExp [(Pos, Exp)]
         | AssignExp Pos Var Exp
         | IfExp IfExp'
         | WhileExp WhileExp'
         | ForExp ForExp'
         | BreakExp Pos
         | LetExp LetExp'
         | ArrayExp ArrayExp'
         deriving (Show)

data CallExp' = CallExp'
  { _pos  :: Pos
  , _func :: Symbol
  , _args :: [Exp]
  } deriving (Show)

data RecordExp' = RecordExp'
  { _pos    :: Pos
  , _type   :: Symbol
  , _fields :: [(Pos, Symbol, Exp)]
  } deriving (Show)

data IfExp' = IfExp'
  { _pos   :: Pos
  , _test  :: Exp
  , _then' :: Exp
  , _else' :: Maybe Exp
  } deriving (Show)

data WhileExp' = WhileExp'
  { _pos  :: Pos
  , _test :: Exp
  , _body :: Exp
  } deriving (Show)

data ForExp' = ForExp'
  { _pos    :: Pos
  , _var    :: Symbol
  , _escape :: Escape
  , _lo     :: Exp
  , _hi     :: Exp
  , _body   :: Exp
  } deriving (Show)

data LetExp' = LetExp'
  { _pos  :: Pos
  , _decs :: [Dec]
  , _body :: Exp
  } deriving (Show)

data ArrayExp' = ArrayExp'
  { _pos  :: Pos
  , _type :: Symbol
  , _size :: Exp
  , _init :: Exp
  } deriving (Show)

-- TODO(DarinM223): change to be multiple decs instead of a single dec.
data Dec = FunctionDec [FunDec]
         | VarDec VarDec'
         | TypeDec [TypeDec']
         deriving (Show)

data Field = Field
  { _pos    :: Pos
  , _name   :: Symbol
  , _type   :: Symbol
  , _escape :: Escape
  } deriving (Show)

data FunDec = FunDec
  { _pos    :: Pos
  , _name   :: Symbol
  , _params :: [Field]
  , _result :: Maybe (Pos, Symbol)
  , _body   :: Exp
  } deriving (Show)

data VarDec' = VarDec'
  { _pos    :: Pos
  , _name   :: Symbol
  , _type   :: Maybe (Pos, Symbol)
  , _init   :: Exp
  , _escape :: Escape
  } deriving (Show)

data TypeDec' = TypeDec'
  { _pos  :: Pos
  , _name :: Symbol
  , _ty   :: Ty
  } deriving (Show)

data Ty = NameTy Symbol Pos
        | RecordTy [Field]
        | ArrayTy Symbol Pos
        deriving (Show)

data Op = PlusOp | MinusOp | TimesOp | DivideOp
        | EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp
        | AndOp | OrOp
        deriving (Show, Eq)
