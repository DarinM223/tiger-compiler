module Chap2.Lexer where

import Control.Monad (void)
import Data.Maybe (fromJust)
import Data.Char (chr)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space (space1 <|> void tab) empty blockCmnt
  where
    blockCmnt = L.skipBlockCommentNested "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

rws :: [String]
rws = [ "array", "if", "then", "else", "while", "for", "to", "do", "let", "in"
      , "end", "of", "break", "nil", "function", "var", "type", "import"
      , "primitive" ]

rword :: String -> Parser ()
rword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

identifier :: Parser String
identifier = (lexeme . try) (parse >>= check)
  where
    parse = string "_main" <|> letters
    letters = (:) <$> letterChar <*> many (alphaNumChar <|> char '_')
    check s | s `elem` rws = fail $ "keyword " ++ show s ++
                             " cannot be an identifier"
            | otherwise    = return s

eol' :: Parser ()
eol' = void (string "\r\n")
   <|> void (string "\n\r")
   <|> void (char '\r')
   <|> void (char '\n')
   <?> "end of line"

string'' :: Parser String
string'' = char '"' *> many character <* char '"'
  where
    character = escape <|> satisfy (/= '\"') 
    escape = char '\\' *> escape'
      where escape' = (char 'x' *> L.hexadecimal >>= check)
                  <|> (L.octal >>= check)
                  <|> oneOf ("\\\"" :: String)
                  <|> fmap (fromJust . toCtrlChar) (oneOf ctrlChars)
                  <?> "Invalid escape sequence"
    check n | n > 255 || n < 0 = fail $ "Number outside of valid range"
            | otherwise        = return $ chr n

toCtrlChar :: Char -> Maybe Char
toCtrlChar c = lookup c escapeLookupTable

ctrlChars :: String
ctrlChars = "abfnrtv"

escapeLookupTable :: [(Char, Char)]
escapeLookupTable = [ ('a', '\a'), ('b', '\b'), ('f', '\f'), ('n', '\n')
                    , ('r', '\r'), ('t', '\t'), ('v', '\v') ]

whileParser :: Parser String
whileParser = sc *> string'' <* eof
