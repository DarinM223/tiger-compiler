module Chap3.Parser where

import Chap2.Lexer
import Chap3.AST
import Chap5.Symbol
import Control.Monad.Combinators.Expr
import Control.Monad.Reader
import Text.Megaparsec hiding (Pos)
import Text.Megaparsec.Char

sepCh :: Char -> Parser ()
sepCh c = sc *> char c *> sc

typedec :: Parser TypeDec'
typedec = TypeDec'
      <$> getSourcePos
      <*> (rword "type" *> ident)
      <*> (symbol "=" *> ty)

ty :: Parser Ty
ty = getSourcePos >>= \pos
  -> (ArrayTy <$> (rword "array" *> rword "of" *> ident) <*> pure pos)
 <|> (RecordTy <$> (symbol "{" *> sepBy tyfield (sepCh ',') <* symbol "}"))
 <|> (NameTy <$> ident <*> pure pos)

tyfield :: Parser Field
tyfield = Field
      <$> getSourcePos
      <*> ident
      <*> (symbol ":" *> ident)
      <*> liftIO mkEscape

vardec :: Parser VarDec'
vardec = VarDec'
     <$> getSourcePos
     <*> (rword "var" *> ident)
     <*> optional (try annot)
     <*> (symbol ":=" *> expr)
     <*> liftIO mkEscape

fundec :: Parser FunDec
fundec = FunDec
     <$> getSourcePos
     <*> (rword "function" *> ident)
     <*> (symbol "(" *> sepBy tyfield (sepCh ',') <* symbol ")")
     <*> optional (try annot)
     <*> (symbol "=" *> expr)

dec :: Parser Dec
dec = (FunctionDec <$> sepBy1 fundec sc)
  <|> (VarDec <$> vardec)
  <|> (TypeDec <$> sepBy1 typedec sc)

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
lfvar = getSourcePos >>= \pos -> LFVarId <$> ident <*> pure pos <*> lfvar'
 where
  lfvar' = getSourcePos >>= \pos
        -> (LFVarId' <$> (symbol "." *> ident) <*> pure pos <*> lfvar')
       <|> (LFVarExpr <$> (symbol "[" *> expr <* symbol "]")
                      <*> pure pos
                      <*> lfvar')
       <|> pure LFVarNil

var :: Parser Var
var = toVar <$> lfvar

call :: Parser CallExp'
call = CallExp'
   <$> getSourcePos
   <*> ident
   <*> (symbol "(" *> sepBy expr (sepCh ',') <* symbol ")")

seq' :: Parser [(Pos, Exp)]
seq' = sepBy parseExp (sepCh ';')
 where
  parseExp = (,) <$> getSourcePos <*> expr

opExp :: Parser Exp
opExp = makeExprParser term operators
 where
  term = sc *> term' <* sc
  term' = (RecordExp <$> try record)
      <|> (NilExp <$ rword "nil")
      <|> (IntExp <$> integer)
      <|> (StringExp <$> getSourcePos <*> string'')
      <|> (CallExp <$> try call)
      <|> (VarExp <$> var)
      <|> (symbol "(" *> expr <* symbol ")")

  unOp op parser = Prefix $
    (\pos exp -> OpExp (IntExp 0) op exp pos) <$> (getSourcePos <* parser)
  binOp ifix op parser = ifix $
    (\pos exp1 exp2 -> OpExp exp1 op exp2 pos) <$> (getSourcePos <* parser)

  dupPrefixOp sym diff = (lexeme . try) (string sym *> notFollowedBy diff)

  operators = [ [unOp MinusOp (symbol "-")]
              , [binOp InfixL TimesOp (symbol "*")]
              , [binOp InfixL DivideOp (symbol "/")]
              , [binOp InfixL PlusOp (symbol "+")]
              , [binOp InfixL MinusOp (symbol "-")]
              , [binOp InfixN EqOp (symbol "=")]
              , [binOp InfixN NeqOp (symbol "<>")]
              , [binOp InfixN GtOp (dupPrefixOp ">" (char '='))]
              , [binOp InfixN LtOp (dupPrefixOp "<" (char '='))]
              , [binOp InfixN GeOp (symbol ">=")]
              , [binOp InfixN LeOp (symbol "<=")]
              -- TODO(DarinM223): figure out what infix & and | are
              -- supposed to be (it does not clearly state in the spec).
              , [binOp InfixL AndOp (symbol "&")]
              , [binOp InfixL OrOp (symbol "|")]
              ]

record :: Parser RecordExp'
record = RecordExp'
  <$> getSourcePos
  <*> ident
  <*> (symbol "{" *> sepBy recfield (sepCh ',') <* symbol "}")
 where
  recfield = (,,) <$> getSourcePos <*> ident <*> (symbol "=" *> expr)

array :: Parser ArrayExp'
array = ArrayExp'
    <$> getSourcePos
    <*> ident
    <*> (symbol "[" *> expr <* symbol "]")
    <*> (rword "of" *> expr)

assign :: Parser Exp
assign = AssignExp <$> getSourcePos <*> var <*> (symbol ":=" *> expr)

ifExp :: Parser IfExp'
ifExp = IfExp'
    <$> getSourcePos
    <*> (rword "if" *> expr)
    <*> (rword "then" *> expr)
    <*> optional (rword "else" *> expr)

while :: Parser WhileExp'
while = WhileExp'
    <$> getSourcePos
    <*> (rword "while" *> expr)
    <*> (rword "do" *> expr)

for :: Parser ForExp'
for = ForExp'
  <$> getSourcePos
  <*> (rword "for" *> ident)
  <*> liftIO mkEscape
  <*> (symbol ":=" *> expr)
  <*> (rword "to" *> expr)
  <*> (rword "do" *> expr)

letExp :: Parser LetExp'
letExp = LetExp'
     <$> getSourcePos
     <*> (rword "let" *> sepBy1 dec sc)
     <*> (SeqExp <$> (rword "in" *> seq' <* rword "end"))

expr :: Parser Exp
expr = (BreakExp <$> getSourcePos <* rword "break")
   <|> (IfExp <$> ifExp)
   <|> (WhileExp <$> while)
   <|> (ForExp <$> for)
   <|> (LetExp <$> letExp)
   <|> (ArrayExp <$> try array)
   <|> try assign
   <|> try opExp
   <|> (RecordExp <$> try record)
   <|> try (symbol "(" *> expr <* symbol ")")
   <|> (NilExp <$ rword "nil")
   <|> (CallExp <$> try call)
   <|> (IntExp <$> integer)
   <|> (StringExp <$> getSourcePos <*> string'')
   <|> (SeqExp <$> (symbol "(" *> seq' <* symbol ")"))
   <|> (VarExp <$> var)

annot :: Parser (Pos, Symbol)
annot = (,) <$> getSourcePos <*> (symbol ":" *> ident)

ident :: Parser Symbol
ident = do
  config <- lift ask
  identifier >>= toSymbol' config

parseExpr :: Parser Exp
parseExpr = sc *> expr <* eof
