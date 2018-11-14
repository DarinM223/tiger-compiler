{-# LANGUAGE QuasiQuotes #-}

module Chap3.Parser where

import Chap2.Lexer
import Chap3.AST
import Chap5.Symbol
import Control.Monad.IO.Class
import Control.Monad.Combinators.Expr
import Data.Void
import NeatInterpolation
import Text.Megaparsec hiding (Pos)
import Text.Megaparsec.Char

import qualified Data.Text as T

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

dec :: Parser Dec
dec = (FunctionDec <$> fundec)
  <|> (VarDec <$> vardec)
  <|> (TypeDec <$> typedec)

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
  term' = (NilExp <$ rword "nil")
      <|> (IntExp <$> integer)
      <|> (StringExp <$> getPosition <*> string'')
      <|> (CallExp <$> try call)
      <|> (VarExp <$> var)
      <|> (symbol "(" *> expr <* symbol ")")

  unOp op parser = Prefix $
    (\pos exp -> OpExp (IntExp 0) op exp pos) <$> (getPosition <* parser)
  binOp ifix op parser = ifix $
    (\pos exp1 exp2 -> OpExp exp1 op exp2 pos) <$> (getPosition <* parser)

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

while :: Parser WhileExp'
while = WhileExp'
    <$> getPosition
    <*> (rword "while" *> expr)
    <*> (rword "do" *> expr)

for :: Parser ForExp'
for = ForExp'
  <$> getPosition
  <*> (rword "for" *> ident)
  <*> liftIO mkEscape
  <*> (symbol ":=" *> expr)
  <*> (rword "to" *> expr)
  <*> (rword "do" *> expr)

letExp :: Parser LetExp'
letExp = LetExp'
     <$> getPosition
     <*> (rword "let" *> sepBy1 dec sc)
     <*> (SeqExp <$> (rword "in" *> seq' <* rword "end"))

expr :: Parser Exp
expr = (NilExp <$ rword "nil")
   <|> (BreakExp <$> getPosition <* rword "break")
   <|> (IfExp <$> ifExp)
   <|> (WhileExp <$> while)
   <|> (ForExp <$> for)
   <|> (LetExp <$> letExp)
   <|> (ArrayExp <$> try array)
   <|> (RecordExp <$> try record)
   <|> try (symbol "(" *> expr <* symbol ")")
   <|> try assign
   <|> try opExp
   <|> (CallExp <$> try call)
   <|> (IntExp <$> integer)
   <|> (StringExp <$> getPosition <*> string'')
   <|> (SeqExp <$> (symbol "(" *> seq' <* symbol ")"))
   <|> (VarExp <$> var)

annot :: Parser (Pos, Symbol)
annot = (,) <$> getPosition <*> (symbol ":" *> ident)

ident :: Parser Symbol
ident = identifier >>= toSymbol

parseExpr :: Parser Exp
parseExpr = sc *> expr <* eof

-- This should not return Left.
testParseCall :: IO (Either (ParseError (Token String) Void) Exp)
testParseCall = runMyParserT
  parseExpr
  "foo(hello.bar, blah[boo], hello.world[bob])"

sample1 :: String
sample1 = T.unpack
  [text|
    let
      var N := 8

      type intArray = array of int

      var row := intArray [ N ] of 0
      var col := intArray [ N ] of 0
      var diag1 := intArray [N+N-1] of 0
      var diag2 := intArray [N+N-1] of 0

      function printboard() =
        (for i := 0 to N-1
          do (for j := 0 to N-1
               do print(if col[i]=j then " O" else " .");
             print("\n"));
          print("\n"))

      function try(c:int) =
        if c=N
        then printboard()
        else for r := 0 to N-1
              do if row[r]=0 & diag1[r+c]=0 & diag2[r+7-c]=0
                 then (row[r]:=1; diag1[r+c]:=1; diag2[r+7-c]:=1;
                       col[c]:=r;
                       try(c+1);
                       row[r]:=0; diag1[r+c]:=0; diag2[r+7-c]:=0)
      in try(0)
    end
  |]

testSample1 :: IO (Either (ParseError (Token String) Void) Exp)
testSample1 = runMyParserT
  parseExpr
  sample1

sample2 :: String
sample2 = T.unpack
  [text|
    let   type any = {any: int}
          var buffer := getchar()

      function readint(any: any) : int =
       let var i := 0
           function isdigit(s : string) : int =
              ord(buffer)>=ord("0") & ord(buffer)<=ord("9")
        in while buffer=" " | buffer="\n" do buffer := getchar();
           any.any := isdigit(buffer);
           while isdigit(buffer)
            do (i := i*10+ord(buffer)-ord("0");
                buffer := getchar());
           i
      end

      type list = {first: int, rest: list}

      function readlist() : list =
          let var any := any{any=0}
              var i := readint(any)
           in if any.any
               then list{first=i,rest=readlist()}
               else (buffer := getchar(); nil)
          end

      function merge(a: list, b: list) : list =
        if a=nil then b
        else if b=nil then a
        else if a.first < b.first
            then list{first=a.first,rest=merge(a.rest,b)}
            else list{first=b.first,rest=merge(a,b.rest)}

      function printint(i: int) =
       let function f(i:int) = if i>0
               then (f(i/10); print(chr(i-i/10*10+ord("0"))))
        in if i<0 then (print("-"); f(-i))
           else if i>0 then f(i)
           else print("0")
       end

      function printlist(l: list) =
        if l=nil then print("\n")
        else (printint(l.first); print(" "); printlist(l.rest))

      in printlist(merge(readlist(), readlist()))
    end
  |]

testSample2 :: IO (Either (ParseError (Token String) Void) Exp)
testSample2 = runMyParserT
  parseExpr
  sample2
