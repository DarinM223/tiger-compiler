{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
module Chap3.AST where

import Prelude hiding (exp, init)
import Chap2.Ref
import Chap5.Symbol

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

convertEscape :: Functor m
              => (r Bool -> m (r' Bool))
              -> Escape r
              -> m (Escape r')
convertEscape f (Escape ref) = Escape <$> f ref

convertExp :: Applicative m => (r Bool -> m (r' Bool)) -> Exp r -> m (Exp r')
convertExp f = \case
  VarExp var      -> VarExp <$> convertVar f var
  NilExp          -> pure NilExp
  StringExp pos s -> pure $ StringExp pos s
  IntExp i        -> pure $ IntExp i
  CallExp (CallExp' pos fn args) ->
    CallExp . CallExp' pos fn <$> traverse (convertExp f) args
  OpExp exp1 op exp2 pos ->
    OpExp <$> convertExp f exp1 <*> pure op <*> convertExp f exp2 <*> pure pos
  RecordExp (RecordExp' pos ty fs) ->
    RecordExp . RecordExp' pos ty
      <$> traverse (\(p, s, e) -> (p, s,) <$> convertExp f e) fs
  SeqExp exps ->
    SeqExp <$> traverse (\(p, e) -> (p,) <$> convertExp f e) exps
  AssignExp pos var exp ->
    AssignExp pos <$> convertVar f var <*> convertExp f exp
  IfExp (IfExp' pos test thn els) ->
    fmap IfExp $ IfExp' pos
      <$> convertExp f test <*> convertExp f thn <*> traverse (convertExp f) els
  WhileExp (WhileExp' pos test body) ->
    fmap WhileExp $ WhileExp' pos <$> convertExp f test <*>  convertExp f body
  ForExp (ForExp' pos var esc lo hi body) ->
    fmap ForExp $ ForExp' pos var
      <$> convertEscape f esc
      <*> convertExp f lo
      <*> convertExp f hi
      <*> convertExp f body
  BreakExp pos -> pure $ BreakExp pos
  LetExp (LetExp' pos decs body) ->
    fmap LetExp $ LetExp' pos
      <$> traverse (convertDec f) decs <*> convertExp f body
  ArrayExp (ArrayExp' pos ty size init) ->
    fmap ArrayExp $ ArrayExp' pos ty <$> convertExp f size <*> convertExp f init

convertVar :: Applicative m => (r Bool -> m (r' Bool)) -> Var r -> m (Var r')
convertVar f = \case
  SimpleVar sym pos -> pure $ SimpleVar sym pos
  FieldVar var sym pos ->
    FieldVar <$> convertVar f var <*> pure sym <*> pure pos
  SubscriptVar var exp pos ->
    SubscriptVar <$> convertVar f var <*> convertExp f exp <*> pure pos

convertDec :: Applicative m => (r Bool -> m (r' Bool)) -> Dec r -> m (Dec r')
convertDec f = \case
  FunctionDec funDecs -> FunctionDec <$> traverse convertFunDec funDecs
  VarDec (VarDec' pos name ty init esc) ->
    fmap VarDec $ VarDec' pos name ty
      <$> convertExp f init
      <*> convertEscape f esc
  TypeDec tys -> TypeDec <$> traverse convertTypeDec tys
 where
  convertFunDec (FunDec pos name params result body) = FunDec pos name
    <$> traverse (convertField f) params <*> pure result <*> convertExp f body
  convertTypeDec (TypeDec' pos name ty) = TypeDec' pos name <$> convertTy f ty

convertField :: Applicative m
             => (r Bool -> m (r' Bool))
             -> Field r
             -> m (Field r')
convertField f (Field pos name ty esc) =
  Field pos name ty <$> convertEscape f esc

convertTy :: Applicative m => (r Bool -> m (r' Bool)) -> Ty r -> m (Ty r')
convertTy f = \case
  NameTy sym pos  -> pure $ NameTy sym pos
  ArrayTy sym pos -> pure $ ArrayTy sym pos
  RecordTy fields -> RecordTy <$> traverse (convertField f) fields
