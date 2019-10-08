{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module ST where

import Chap2.Lexer hiding (mkContextData)
import Chap2.Ref
import Chap3.AST (Exp, convertExp)
import Chap3.Parser (parseExpr)
import Chap5.Symbol hiding (mkSymbolTableM)
import Chap6.Temp
import Control.Monad.ST
import Data.STRef
import Text.Megaparsec (runParserT)

import qualified Data.HashTable.Class as H
import qualified Data.HashTable.ST.Basic as HB

type instance Ref (ST s) = STRef s

mkSymbolTableM :: ST s (SymbolTableM String Int (ST s))
mkSymbolTableM = do
  table <- HB.new
  return SymbolTableM
    { insertTable = H.insert table
    , lookupTable = H.lookup table
    , tableToList = H.toList table
    }

mkContextData :: ST s (ParserContextData (ST s))
mkContextData = do
  symbolM <- mkSymbolM refM <$> mkSymbolTableM <*> mkSymbolRef refM
  tempRef <- mkTempRef refM
  return $ ParserContextData symbolM refM tempRef
 where
  refM = RefM { newRef = newSTRef, readRef = readSTRef, writeRef = writeSTRef }

newtype BoolValue a = BoolValue Bool deriving (Show, Eq)

convert :: STRef s Bool -> ST s (BoolValue Bool)
convert ref = BoolValue <$> readSTRef ref

runSTParser :: (ParserContext (ST s) => Parser (ST s) a)
            -> String
            -> ST s (Either ParseErr a)
runSTParser m s = mkContextData >>= \d -> withContextData d $ runParserT m "" s

parse :: String -> Either ParseErr (Exp BoolValue)
parse s = runST $ runSTParser parseExpr s >>=
  either (pure . Left) (fmap Right . convertExp convert)
