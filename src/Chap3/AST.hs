module Chap3.AST where

import Data.IORef
import Text.Megaparsec (SourcePos)

newtype Symbol = Symbol String deriving (Show, Eq)
type Pos = SourcePos

data Var = SimpleVar Symbol Pos
         | FieldVar Var Symbol Pos
         | SubscriptVar Var Exp Pos

data Exp = VarExp Var
         | NilExp
         | IntExp Int
         | StringExp String Pos
         | CallExp CallExp'
         | OpExp Exp Op Exp Pos
         | RecordExp RecordExp'
         | SeqExp [(Exp, Pos)]
         | AssignExp Var Exp Pos
         | IfExp IfExp'
         | WhileExp WhileExp'
         | ForExp ForExp'
         | BreakExp Pos
         | LetExp LetExp'
         | ArrayExp ArrayExp'

data CallExp' = CallExp'
  { _func :: Symbol
  , _args :: [Exp]
  , _pos  :: Pos
  }

data RecordExp' = RecordExp'
  { _fields :: [(Symbol, Exp, Pos)]
  , _type   :: Symbol
  , _pos    :: Pos
  }

data IfExp' = IfExp'
  { _test  :: Exp
  , _then' :: Exp
  , _else' :: Maybe Exp
  , _pos   :: Pos
  }

data WhileExp' = WhileExp'
  { _test :: Exp
  , _body :: Exp
  , _pos  :: Pos
  }

data ForExp' = ForExp'
  { _var    :: Symbol
  , _escape :: IORef Bool
  , _lo     :: Exp
  , _hi     :: Exp
  , _body   :: Exp
  , _pos    :: Pos
  }

data LetExp' = LetExp'
  { _decs :: [Dec]
  , _body :: Exp
  , _pos  :: Pos
  }

data ArrayExp' = ArrayExp'
  { _type :: Symbol
  , _size :: Exp
  , _init :: Exp
  , _pos  :: Pos
  }

data Dec = FunctionDec [FunDec]
         | VarDec VarDec'
         | TypeDec [TypeDec']

data Field = Field
  { _name   :: Symbol
  , _type   :: Symbol
  , _escape :: IORef Bool
  , _pos    :: Pos
  }

data FunDec = FunDec
  { _name   :: Symbol
  , _params :: [Field]
  , _result :: Maybe (Symbol, Pos)
  , _body   :: Exp
  , _pos    :: Pos
  }

data VarDec' = VarDec'
  { _name   :: Symbol
  , _type   :: Maybe (Symbol, Pos)
  , _init   :: Exp
  , _escape :: IORef Bool
  , _pos    :: Pos
  }

data TypeDec' = TypeDec'
  { _name :: Symbol
  , _ty   :: Ty
  , _pos  :: Pos
  }

data Ty = NameTy Symbol Pos
        | RecordTy [Field]
        | ArrayTy Symbol Pos

data Op = PlusOp | MinusOp | TimesOp | DivideOp
        | EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp
        deriving (Show, Eq)
