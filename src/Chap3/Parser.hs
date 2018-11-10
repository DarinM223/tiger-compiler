module Chap3.Parser where

import Chap2.Lexer
import Chap3.AST
import Control.Monad.IO.Class
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
      <*> (liftIO $ mkEscape)
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
                       <*> (liftIO $ mkEscape)
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

-- Left factoring grammar:
--
-- lvalue -> id
--        -> lvalue . id
--        -> lvalue [exp]
--
-- b = { id }
-- a = { id, [exp }
--
-- A -> b1A' | ... | bmA'
-- A' -> a1A' | ... | anA' | e
--
-- A -> id A'
-- A' -> id A' | [exp] A' | e
lvalue :: Parser Var
lvalue = getPosition >>= \pos
      -> (SimpleVar <$> ident <*> pure pos)
     <|> (FieldVar <$> lvalue <*> (symbol "." *> ident) <*> pure pos)
     <|> (SubscriptVar <$> lvalue
                       <*> (symbol "[" *> expr <* symbol "]")
                       <*> pure pos)

call :: Parser CallExp'
call = getPosition >>= \pos
    -> CallExp'
   <$> ident
   <*> (symbol "(" *> sepBy expr (sc *> char ',' *> sc) <* symbol ")")
   <*> pure pos

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
expr = getPosition >>= \pos
    -> (NilExp <$ rword "nil")
   <|> (IntExp <$> integer)
   <|> (StringExp <$> string'' <*> pure pos)
   <|> (CallExp <$> try call)
   <|> (VarExp <$> lvalue)

annot :: Parser (Symbol, Pos)
annot = getPosition >>= \pos -> (,) <$> (symbol ":" *> ident) <*> pure pos

ident :: Parser Symbol
ident = Symbol <$> identifier

parseExpr :: Parser Exp
parseExpr = sc *> expr <* eof
