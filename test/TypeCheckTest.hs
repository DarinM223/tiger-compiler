module TypeCheckTest (tests) where

import Control.Monad (forM, forM_, when)
import Control.Monad.Catch
import Chap5.Semant
import Chap5.Symbol
import Chap5.Table
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.HashMap.Strict as HM

tests :: TestTree
tests = testGroup "Type checking tests"
  [ testCase "Tests let expression" $
    testExpTy testLet (ExpTy EUnit TInt) False
  , testCase "Tests call expression" $
    testExpTy testCall (ExpTy EUnit TInt) False
  , testCase "Tests records" $ testRecord False
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

testRecord :: Bool -> IO ()
testRecord debug = do
  results <- forM tests $ \test -> catch (justTuple <$> testTySyms test) $
    \(_ :: SomeException) -> return (HM.empty, Nothing)
  forM_ (zip results expected) $ \((table, expty), expect) -> do
    when debug $ print expty
    expty @?= expect table
 where
  justTuple (a, b) = (a, Just b)
  tests =
    [ "let type rec = { a: int, b: string } in rec { a = 1, b = \"hello\" } end"
    , "let type rec = { a: int, b: string } in rec { a = 1, b = \"hello\", b = \"world\" } end"
    , "let type rec = { a: int, b: string } in rec { a = 1, c = \"hello\" } end"
    , "let type rec = { c: int, d: string } in rec { d = \"hello\", c = 1 } end"
    , "let type rec = { c: int } in rec { } end" -- TODO(DarinM223): should this fail?
    ]
  lookup s table = Symbol (s, table HM.! s)
  expected =
    [ \t -> Just $ ExpTy EUnit
      (TRecord [(lookup "a" t, TInt), (lookup "b" t, TString)] UniqueIgnore)
    , const Nothing
    , const Nothing
    , \t -> Just $ ExpTy EUnit
      (TRecord [(lookup "c" t, TInt), (lookup "d" t, TString)] UniqueIgnore)
    , const Nothing
    ]
