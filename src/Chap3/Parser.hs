module Chap3.Parser where

import Chap2.Lexer
import Chap3.AST
import Control.Monad.IO.Class
import Data.IORef
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
      <*> (liftIO $ newIORef False)
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
                       <*> (liftIO $ newIORef False)
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

expr :: Parser Exp
expr = undefined

annot :: Parser (Symbol, Pos)
annot = getPosition >>= \pos -> (,) <$> (symbol ":" *> ident) <*> pure pos

ident :: Parser Symbol
ident = undefined
