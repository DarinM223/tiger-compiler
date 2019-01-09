module Chap2.Lexer where

import Control.Monad (void)
import Control.Monad.Reader
import Chap5.Symbol
import Chap6.Temp
import Data.Maybe (fromJust)
import Data.Char (chr, ord)
import Data.Void
import GHC.Generics
import Text.Megaparsec
import Text.Megaparsec.Char

import qualified Text.Megaparsec.Char.Lexer as L

data Config = Config
  { _symRef   :: SymbolRef
  , _symTable :: SymbolTable
  , _tempRef  :: TempRef
  } deriving Generic

mkConfig :: IO Config
mkConfig = Config <$> mkSymbolRef <*> mkSymbolTable <*> mkTempRef

type Parser = ParsecT Void String (ReaderT Config IO)
type ParseErr = ParseErrorBundle String Void

runMyParserT :: Parser a
             -> String
             -> IO (Either ParseErr a)
runMyParserT m s = do
  config <- mkConfig
  flip runReaderT config $ runParserT m "" s

sc :: Parser ()
sc = L.space (space1 <|> void tab) empty blockCmnt
 where blockCmnt = L.skipBlockCommentNested "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

rws :: [String]
rws = [ "while", "for", "to", "break", "let", "in", "end", "function", "var"
      , "type", "array", "if", "then", "else", "do", "of", "nil" ]

rword :: String -> Parser ()
rword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

identifier :: Parser String
identifier = (lexeme . try) (letters >>= check)
 where
  letters = (:) <$> letterChar <*> many (alphaNumChar <|> char '_')
  check s
    | s `elem` rws = fail $ "keyword " ++ show s ++ " cannot be an identifier"
    | otherwise    = return s

eol' :: Parser ()
eol' = void (string "\r\n")
   <|> void (string "\n\r")
   <|> void (char '\r')
   <|> void (char '\n')
   <?> "end of line"

integer :: Parser Int
integer = L.decimal

string'' :: Parser String
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

whileParser :: Parser [String]
whileParser = sc *> sepBy identifier (sc *> char ',' *> sc) <* eof
