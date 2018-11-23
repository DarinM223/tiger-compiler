module TypeCheckTest (tests) where

import Control.Monad (forM_, when)
import Control.Monad.Catch
import Chap5.Semant
import Chap5.Symbol
import Chap5.Table hiding (look)
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.HashMap.Strict as HM

tests :: TestTree
tests = testGroup "Type checking tests"
  [ testCase "Tests let expression" $
    testExpTy testLet (ExpTy EUnit TInt) False
  , testCase "Tests call expression" $
    testExpTy testCall (ExpTy EUnit TInt) False
  , testCase "Tests records" $ testRecord
  ]

testExpTy :: IO ExpTy -> ExpTy -> Bool -> IO ()
testExpTy m comp debug = do
  result <- m
  when debug $ print result
  result @?= comp

testLet :: IO ExpTy
testLet = testTy "let type a = int\n var a : a := 2 in a end"

testCall :: IO ExpTy
testCall = testTy $ "let function add(a : int, b: int): int = a + b\n" ++
                    "var a : int := 2\n var b := 3 in add(a, b) end"

-- | Creates table driven tests given a list of test strings
-- and a list of the expected output.
testTable :: [(Bool, String)]                         -- ^ Tests
          -> [(HM.HashMap String Int -> Maybe ExpTy)] -- ^ Expected
          -> IO ()
testTable tests expected = do
  results <- traverse runTest tests
  forM_ (zip results expected) $ \((debug, table, expty), expect) -> do
    when debug $ print expty
    expty @?= expect table
 where
  justTuple debug (a, b) = (debug, a, Just b)
  runTest (debug, test) = catch (justTuple debug <$> testTySyms test) $
    \(e :: SomeException) -> do
      when debug $ print e
      return (debug, HM.empty, Nothing)

-- | Looks up a symbol from the table.
look :: String -> HM.HashMap String Int -> Symbol
look s table = Symbol (s, table HM.! s)

testRecord :: IO ()
testRecord = testTable tests expected
 where
  tests =
    [ (False, "let type rec = { a: int, b: string } in rec { a = 1, b = \"hello\" } end")
    , (False, "let type rec = { a: int, b: string } in rec { a = 1, b = \"hello\", b = \"world\" } end")
    , (False, "let type rec = { a: int, b: string } in rec { a = 1, c = \"hello\" } end")
    , (False, "let type rec = { c: int, d: string } in rec { d = \"hello\", c = 1 } end")
    , (False, "let type rec = { c: int } in rec { } end")
    , (False, "let type rec = { } in rec { } end")
    ]
  expected =
    [ \t -> Just $ ExpTy EUnit $
      TRecord [(look "a" t, TInt), (look "b" t, TString)] UniqueIgnore
    , const Nothing
    , const Nothing
    , \t -> Just $ ExpTy EUnit $
      TRecord [(look "c" t, TInt), (look "d" t, TString)] UniqueIgnore
    , const Nothing
    , const $ Just $ ExpTy EUnit $ TRecord [] UniqueIgnore
    ]
