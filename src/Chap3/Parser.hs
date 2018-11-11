module Chap3.Parser where

import Chap2.Lexer
import Chap3.AST
import Control.Monad.IO.Class
import Control.Monad.Combinators.Expr
import Data.Void
import Text.Megaparsec hiding (Pos)
import Text.Megaparsec.Char

typedec :: Parser TypeDec'
typedec = getPosition >>= \pos
       -> TypeDec'
      <$> (rword "type" *> ident)
      <*> (symbol "=" *> ty)
      <*> pure pos

ty :: Parser Ty
ty = getPosition >>= \pos
  -> (ArrayTy <$> (rword "array" *> rword "of" *> ident) <*> pure pos)
 <|> (RecordTy <$> (symbol "{" *> sepBy1 tyfield (char ',') <* symbol "}"))
 <|> (NameTy <$> ident <*> pure pos)

tyfield :: Parser Field
tyfield = getPosition >>= \pos
       -> Field
      <$> ident
      <*> ident
      <*> liftIO mkEscape
      <*> pure pos

vardec :: Parser VarDec'
vardec = getPosition >>= \pos
      -> vardec' (Just <$> annot) pos
     <|> vardec' (pure Nothing) pos
 where
  vardec' parseAnnot pos = VarDec'
                       <$> (rword "var" *> ident)
                       <*> parseAnnot
                       <*> (symbol ":=" *> expr)
                       <*> liftIO mkEscape
                       <*> pure pos

fundec :: Parser FunDec
fundec = getPosition >>= \pos
      -> fundec' (Just <$> annot) pos
     <|> fundec' (pure Nothing) pos
 where
  fundec' parseAnnot pos = FunDec
    <$> (rword "function" *> ident)
    <*> (symbol "(" *> sepBy1 tyfield (char ',') <* symbol ")")
    <*> parseAnnot
    <*> (symbol "=" *> expr)
    <*> pure pos

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
call = getPosition >>= \pos
    -> CallExp'
   <$> ident
   <*> (symbol "(" *> sepBy expr (sc *> char ',' *> sc) <* symbol ")")
   <*> pure pos

seq' :: Parser [(Exp, Pos)]
seq' = sepBy parseExp (sc *> char ';' *> sc)
 where
  parseExp = getPosition >>= \pos -> (,) <$> expr <*> pure pos

{-data Op = PlusOp | MinusOp | TimesOp | DivideOp-}
{-        | EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp-}
{-        deriving (Show, Eq)-}

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
  binOp op parser = InfixL $
    (\pos exp1 exp2 -> OpExp exp1 op exp2 pos) <$> (getPosition <* parser)

  operators = [ [unOp MinusOp (symbol "-")]
              , [binOp TimesOp (symbol "*")]
              , [binOp DivideOp (symbol "/")]
              , [binOp PlusOp (symbol "+")]
              , [binOp MinusOp (symbol "-")]
              ]

{-data Exp = VarExp Var-}
{-         | NilExp-}
{-         | IntExp Int-}
{-         | StringExp String Pos-}
{-         | CallExp CallExp'-}
{-         | OpExp Exp Op Exp Pos-}
{-         | RecordExp RecordExp'-}
{-         | SeqExp [(Exp, Pos)]-}
{-         | AssignExp Var Exp Pos-}
{-         | IfExp IfExp'-}
{-         | WhileExp WhileExp'-}
{-         | ForExp ForExp'-}
{-         | BreakExp Pos-}
{-         | LetExp LetExp'-}
{-         | ArrayExp ArrayExp'-}
{-         | LValueExp LValue-}

expr :: Parser Exp
expr = (NilExp <$ rword "nil")
   <|> try opExp
   <|> (IntExp <$> integer)
   <|> (StringExp <$> getPosition <*> string'')
   <|> (CallExp <$> try call)
   <|> try (symbol "(" *> expr <* symbol ")")
   <|> (SeqExp <$> (symbol "(" *> seq' <* symbol ")"))
   <|> (VarExp <$> var)

annot :: Parser (Symbol, Pos)
annot = getPosition >>= \pos -> (,) <$> (symbol ":" *> ident) <*> pure pos

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
