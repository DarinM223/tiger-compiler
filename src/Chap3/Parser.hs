module Chap3.Parser where

import Chap2.Lexer
import Chap3.AST
import Control.Monad.IO.Class
import Control.Monad.Combinators.Expr
import Data.Void
import Text.Megaparsec hiding (Pos)
import Text.Megaparsec.Char

sepCh :: Char -> Parser ()
sepCh c = sc *> char c *> sc

typedec :: Parser TypeDec'
typedec = TypeDec'
      <$> getPosition
      <*> (rword "type" *> ident)
      <*> (symbol "=" *> ty)

ty :: Parser Ty
ty = getPosition >>= \pos
  -> (ArrayTy <$> (rword "array" *> rword "of" *> ident) <*> pure pos)
 <|> (RecordTy <$> (symbol "{" *> sepBy1 tyfield (sepCh ',') <* symbol "}"))
 <|> (NameTy <$> ident <*> pure pos)

tyfield :: Parser Field
tyfield = Field
      <$> getPosition
      <*> ident
      <*> (symbol ":" *> ident)
      <*> liftIO mkEscape

vardec :: Parser VarDec'
vardec = VarDec'
     <$> getPosition
     <*> (rword "var" *> ident)
     <*> optional (try annot)
     <*> (symbol ":=" *> expr)
     <*> liftIO mkEscape

fundec :: Parser FunDec
fundec = FunDec
     <$> getPosition
     <*> (rword "function" *> ident)
     <*> (symbol "(" *> sepBy tyfield (sepCh ',') <* symbol ")")
     <*> optional (try annot)
     <*> (symbol "=" *> expr)

-- Left factoring lvalue grammar:
--
-- lvalue -> id
--        -> lvalue . id
--        -> lvalue [exp]
--
-- b = { id }
-- a = { id, exp }
--
-- A -> b1A' | ... | bmA'
-- A' -> a1A' | ... | anA' | e
--
-- A -> id A'
-- A' -> . id A' | [exp] A' | e
data LFVar = LFVarId !Symbol !Pos LFVar' deriving (Show)
data LFVar' = LFVarId' !Symbol !Pos LFVar'
            | LFVarExpr !Exp !Pos LFVar'
            | LFVarNil
            deriving (Show)

-- | Converts the left factored lvalue ast to the proper ast.
toVar :: LFVar -> Var
toVar (LFVarId sym pos rest) = toVar' (SimpleVar sym pos) rest
 where
  -- TODO(DarinM223): make sure this is building strictly (look into deepseq)
  toVar' :: Var -> LFVar' -> Var
  toVar' !build = \case
    LFVarNil                  -> build
    (LFVarId' sym pos rest)   -> toVar' (FieldVar build sym pos) rest
    (LFVarExpr expr pos rest) -> toVar' (SubscriptVar build expr pos) rest

-- | Parses the left factored grammar for lvalues.
lfvar :: Parser LFVar
lfvar = getPosition >>= \pos -> LFVarId <$> ident <*> pure pos <*> lfvar'
 where
  lfvar' = getPosition >>= \pos
        -> (LFVarId' <$> (symbol "." *> ident) <*> pure pos <*> lfvar')
       <|> (LFVarExpr <$> (symbol "[" *> expr <* symbol "]")
                      <*> pure pos
                      <*> lfvar')
       <|> pure LFVarNil

var :: Parser Var
var = toVar <$> lfvar

call :: Parser CallExp'
call = CallExp'
   <$> getPosition
   <*> ident
   <*> (symbol "(" *> sepBy expr (sepCh ',') <* symbol ")")

seq' :: Parser [(Pos, Exp)]
seq' = sepBy parseExp (sepCh ';')
 where
  parseExp = (,) <$> getPosition <*> expr

opExp :: Parser Exp
opExp = makeExprParser term operators
 where
  term = sc *> term' <* sc
  term' = (IntExp <$> integer)
      <|> (StringExp <$> getPosition <*> string'')
      <|> (CallExp <$> try call)
      <|> (VarExp <$> var)
      <|> (symbol "(" *> expr <* symbol ")")

  unOp op parser = Prefix $
    (\pos exp -> OpExp (IntExp 0) op exp pos) <$> (getPosition <* parser)
  binOp ifix op parser = ifix $
    (\pos exp1 exp2 -> OpExp exp1 op exp2 pos) <$> (getPosition <* parser)

  operators = [ [unOp MinusOp (symbol "-")]
              , [binOp InfixL TimesOp (symbol "*")]
              , [binOp InfixL DivideOp (symbol "/")]
              , [binOp InfixL PlusOp (symbol "+")]
              , [binOp InfixL MinusOp (symbol "-")]
              , [binOp InfixN EqOp (symbol "=")]
              , [binOp InfixN NeqOp (symbol "<>")]
              , [binOp InfixN GtOp (symbol ">")]
              , [binOp InfixN LtOp (symbol "<")]
              , [binOp InfixN GeOp (symbol ">=")]
              , [binOp InfixN LeOp (symbol "<=")]
              -- TODO(DarinM223): figure out what infix & and | are
              -- supposed to be (it does not clearly state in the spec).
              , [binOp InfixL AndOp (symbol "&")]
              , [binOp InfixL OrOp (symbol "|")]
              ]

record :: Parser RecordExp'
record = RecordExp'
  <$> getPosition
  <*> ident
  <*> (symbol "{" *> sepBy recfield (sepCh ',') <* symbol "}")
 where
  recfield = (,,) <$> getPosition <*> ident <*> (symbol "=" *> expr)

array :: Parser ArrayExp'
array = ArrayExp'
    <$> getPosition
    <*> ident
    <*> (symbol "[" *> expr <* symbol "]")
    <*> (rword "of" *> expr)

assign :: Parser Exp
assign = AssignExp <$> getPosition <*> var <*> (symbol ":=" *> expr)

ifExp :: Parser IfExp'
ifExp = IfExp'
    <$> getPosition
    <*> (rword "if" *> expr)
    <*> (rword "then" *> expr)
    <*> optional (rword "else" *> expr)

{-data Exp = VarExp Var-}             -- check
{-         | NilExp-}                 -- check
{-         | IntExp Int-}             -- check
{-         | StringExp String Pos-}   -- check
{-         | CallExp CallExp'-}       -- check
{-         | OpExp Exp Op Exp Pos-}   -- check
{-         | RecordExp RecordExp'-}   -- check
{-         | SeqExp [(Exp, Pos)]-}    -- check
{-         | AssignExp Var Exp Pos-}  -- check
{-         | IfExp IfExp'-}           -- check
{-         | WhileExp WhileExp'-}
{-         | ForExp ForExp'-}
{-         | BreakExp Pos-}           -- check
{-         | LetExp LetExp'-}
{-         | ArrayExp ArrayExp'-}     -- check

expr :: Parser Exp
expr = (NilExp <$ rword "nil")
   <|> (BreakExp <$> getPosition <* rword "break")
   <|> (IntExp <$> integer)
   <|> (StringExp <$> getPosition <*> string'')
   <|> (IfExp <$> ifExp)
   <|> (ArrayExp <$> try array)
   <|> (RecordExp <$> try record)
   <|> (CallExp <$> try call)
   <|> try (symbol "(" *> expr <* symbol ")")
   <|> try assign
   <|> try opExp
   <|> (SeqExp <$> (symbol "(" *> seq' <* symbol ")"))
   <|> (VarExp <$> var)

annot :: Parser (Pos, Symbol)
annot = (,) <$> getPosition <*> (symbol ":" *> ident)

ident :: Parser Symbol
ident = Symbol <$> identifier

parseExpr :: Parser Exp
parseExpr = sc *> expr <* eof

-- This should not return Left.
testParseCall :: IO (Either (ParseError (Token String) Void) Exp)
testParseCall = runParserT
  parseExpr
  ""
  "foo(hello.bar, blah[boo], hello.world[bob])"
