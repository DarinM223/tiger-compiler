{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Chap2.Lexer where

import Control.Monad (void)
import Control.Monad.Reader
import Chap3.AST
import Chap5.Symbol
import Chap6.Temp
import Data.Char (chr, ord)
import Data.Functor.Classes (Eq1 (liftEq))
import Data.Maybe (fromJust)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.IORef as IORef
import qualified Text.Megaparsec.Char.Lexer as L

newtype IORef a = IORef { unIORef :: IORef.IORef a } deriving Eq
newIORef v = IORef <$> IORef.newIORef v
readIORef (IORef ref) = IORef.readIORef ref
writeIORef (IORef ref) v = IORef.writeIORef ref v

instance Eq1 IORef where
  -- NOTE(DarinM223): uses unsafeCoerce here because
  -- Eq for IORef only checks for pointer equality
  -- so you can compare IORef a and IORef b even though
  -- they are different types.
  liftEq _ f1 f2 = f1 == unsafeCoerce f2

type family Ref (m :: * -> *) :: * -> *
type instance Ref IO = IORef

type Parser m = ParsecT Void String m
type ParseErr = ParseErrorBundle String Void
type ParserContext m = ( ?symbolM :: SymbolM m
                       , ?refM    :: RefM (Ref m) m
                       , ?tempRef :: TempRef (Ref m)
                       )

data ParserContextData m = ParserContextData
  { symbolM :: SymbolM m
  , refM    :: RefM (Ref m) m
  , tempRef :: TempRef (Ref m)
  }

contextDataIO :: IO (ParserContextData IO)
contextDataIO = do
  symbolM <- liftIO $ mkSymbolM <$> mkSymbolRef <*> mkSymbolTable
  tempRef <- liftIO $ mkTempRef refM
  return $ ParserContextData symbolM refM tempRef
 where
  refM = RefM { newRef = newIORef, readRef = readIORef, writeRef = writeIORef }

withContextData :: ParserContextData m -> (ParserContext m => m a) -> m a
withContextData c f =
  let { ?symbolM = symbolM c; ?refM = refM c; ?tempRef = tempRef c } in f

runMyParserT :: ParserContext IO
             => (ParserContext IO => Parser IO a)
             -> String
             -> IO (Either ParseErr a)
runMyParserT m s = runParserT m "" s

runMyParserT' :: (ParserContext IO => Parser IO a)
              -> String
              -> IO (Either ParseErr a)
runMyParserT' m s = contextDataIO >>= \d -> withContextData d $ runMyParserT m s

sc :: Parser m ()
sc = L.space (space1 <|> void tab) empty blockCmnt
 where blockCmnt = L.skipBlockCommentNested "/*" "*/"

lexeme :: Parser m a -> Parser m a
lexeme = L.lexeme sc

symbol :: String -> Parser m String
symbol = L.symbol sc

rws :: [String]
rws = [ "while", "for", "to", "break", "let", "in", "end", "function", "var"
      , "type", "array", "if", "then", "else", "do", "of", "nil" ]

rword :: String -> Parser m ()
rword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

identifier :: Parser m String
identifier = (lexeme . try) (letters >>= check)
 where
  letters = (:) <$> letterChar <*> many (alphaNumChar <|> char '_')
  check s
    | s `elem` rws = fail $ "keyword " ++ show s ++ " cannot be an identifier"
    | otherwise    = return s

eol' :: Parser m ()
eol' = void (string "\r\n")
   <|> void (string "\n\r")
   <|> void (char '\r')
   <|> void (char '\n')
   <?> "end of line"

integer :: Parser m Int
integer = L.decimal

string'' :: Parser m String
string'' = char '"' *> many character <* char '"'
 where
  character = escape <|> satisfy (/= '\"')
  escape    = char '\\' *> escape'
   where
    escape' = (L.decimal >>= check)
          <|> (char '^' *> ctrlChar)
          <|> fmap (fromJust . toEscChar) (oneOf singleEscChars)
          <|> (many spaceChar *> char '\\' *> character)
          <?> "Invalid escape sequence"
  ctrlChar = fmap (chr . subtract (ord '@') . ord) asciiChar
  check n
    | n > 255 || n < 0 = fail "Number outside of valid range"
    | otherwise        = return $ chr n

toEscChar :: Char -> Maybe Char
toEscChar c = lookup c escapeLookupTable

singleEscChars :: String
singleEscChars = "nt\\\""

escapeLookupTable :: [(Char, Char)]
escapeLookupTable = [('n', '\n'), ('t', '\t'), ('\\', '\\'), ('\"', '\"')]

whileParser :: Parser m [String]
whileParser = sc *> sepBy identifier (sc *> char ',' *> sc) <* eof
