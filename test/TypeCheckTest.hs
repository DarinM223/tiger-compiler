module TypeCheckTest (tests) where

import Control.Monad (when)
import Chap5.Semant
import Chap5.Table
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Type checking tests"
  [ testCase "Tests let expression" $
    testExpTy testLet (ExpTy EUnit TInt) False
  , testCase "Tests call expression" $
    testExpTy testCall (ExpTy EUnit TInt) False
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
