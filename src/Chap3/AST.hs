module Chap3.AST where

import Data.IORef
import Text.Megaparsec (SourcePos)

newtype Symbol = Symbol { unSymbol :: String } deriving (Show, Eq)
type Pos = SourcePos

newtype Escape = Escape { unEscape :: IORef Bool }
instance Show Escape where
    show _ = ""

mkEscape :: IO Escape
mkEscape = Escape <$> (newIORef False)

data Var = SimpleVar Symbol Pos
         | FieldVar Var Symbol Pos
         | SubscriptVar Var Exp Pos
         deriving (Show)

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
         | SymbolExp Symbol
         deriving (Show)

data CallExp' = CallExp'
  { _func :: Symbol
  , _args :: [Exp]
  , _pos  :: Pos
  } deriving (Show)

data RecordExp' = RecordExp'
  { _fields :: [(Symbol, Exp, Pos)]
  , _type   :: Symbol
  , _pos    :: Pos
  } deriving (Show)

data IfExp' = IfExp'
  { _test  :: Exp
  , _then' :: Exp
  , _else' :: Maybe Exp
  , _pos   :: Pos
  } deriving (Show)

data WhileExp' = WhileExp'
  { _test :: Exp
  , _body :: Exp
  , _pos  :: Pos
  } deriving (Show)

data ForExp' = ForExp'
  { _var    :: Symbol
  , _escape :: Escape
  , _lo     :: Exp
  , _hi     :: Exp
  , _body   :: Exp
  , _pos    :: Pos
  } deriving (Show)

data LetExp' = LetExp'
  { _decs :: [Dec]
  , _body :: Exp
  , _pos  :: Pos
  } deriving (Show)

data ArrayExp' = ArrayExp'
  { _type :: Symbol
  , _size :: Exp
  , _init :: Exp
  , _pos  :: Pos
  } deriving (Show)

data Dec = FunctionDec [FunDec]
         | VarDec VarDec'
         | TypeDec [TypeDec']
         deriving (Show)

data Field = Field
  { _name   :: Symbol
  , _type   :: Symbol
  , _escape :: Escape
  , _pos    :: Pos
  } deriving (Show)

data FunDec = FunDec
  { _name   :: Symbol
  , _params :: [Field]
  , _result :: Maybe (Symbol, Pos)
  , _body   :: Exp
  , _pos    :: Pos
  } deriving (Show)

data VarDec' = VarDec'
  { _name   :: Symbol
  , _type   :: Maybe (Symbol, Pos)
  , _init   :: Exp
  , _escape :: Escape
  , _pos    :: Pos
  } deriving (Show)

data TypeDec' = TypeDec'
  { _name :: Symbol
  , _ty   :: Ty
  , _pos  :: Pos
  } deriving (Show)

data Ty = NameTy Symbol Pos
        | RecordTy [Field]
        | ArrayTy Symbol Pos
        deriving (Show)

data Op = PlusOp | MinusOp | TimesOp | DivideOp
        | EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp
        deriving (Show, Eq)
