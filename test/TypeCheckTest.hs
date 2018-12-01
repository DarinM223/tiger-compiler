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
  , testCase "Tests records" testRecord
  , testCase "Test if else" testIfElse
  , testCase "Test while" testWhile
  , testCase "Test for" testFor
  , testCase "Test array" testArray
  , testCase "Test operators" testOps
  , testCase "Test assignment" testAssign
  , testCase "Test recursive declarations" testRec
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

testIfElse :: IO ()
testIfElse = testTable tests expected
 where
  tests =
    [ (False, "if 1 then flush()")
    , (False, "if 1 then flush() else 3")
    , (False, "if 1 then 3")
    , (False, "if \"hello\" then flush()")
    , (False, "let var s := \"h\" in if ord(s) then flush() end")
    , (False, "let var s := \"hello\" in if 1 then size(s) else ord(s) end")
    , (False, "let type mi = int\nvar i : mi := 2 in if i then i else 0 end")
    ]
  expected =
    [ const $ Just $ ExpTy EUnit TUnit
    , const Nothing
    , const Nothing
    , const Nothing
    , const $ Just $ ExpTy EUnit TUnit
    , const $ Just $ ExpTy EUnit TInt
    , const $ Just $ ExpTy EUnit TInt
    ]

testWhile :: IO ()
testWhile = testTable tests expected
 where
  tests =
    [ (False, "while 1 do flush()")
    , (False, "while \"hello\" do flush()")
    , (False, "while 1 do 1")
    ]
  expected =
    [ const $ Just $ ExpTy EUnit TUnit
    , const Nothing
    , const Nothing
    ]

testFor :: IO ()
testFor = testTable tests expected
 where
  tests =
    [ (False, "for i := 0 to 10 do flush()")
    , (False, "for i := 0 to 10 do print(chr(i))")
    , (False, "for i := 0 to 10 do print(i)")
    , (False, "let var i := \"hello\"\n in for i := 0 to 10 do print(chr(i)) end")
    , (False, "for i := 0 to \"hello\" do flush()")
    , (False, "for i := \"hello\" to 10 do flush()")
    , (False, "for i := 0 to 10 do 1")
    , (False, "for i := ord(\"a\") to ord(\"b\") do flush()")
    ]
  expected =
    [ const $ Just $ ExpTy EUnit TUnit
    , const $ Just $ ExpTy EUnit TUnit
    , const Nothing
    , const $ Just $ ExpTy EUnit TUnit
    , const Nothing
    , const Nothing
    , const Nothing
    , const $ Just $ ExpTy EUnit TUnit
    ]

testArray :: IO ()
testArray = testTable tests expected
 where
  tests =
    [ (False, "let type intArray = array of int\nvar a := intArray [10] of 0 in a end")
    , (False, "let type intArray = array of int\nvar a := intArray [\"1\"] of 0 in a end")
    , (False, "let type intArray = int\nvar a := intArray [10] of 0 in a end")
    , (False, "let type ia = array of int\ntype ia2 = ia\nvar a := ia2 [10] of 0 in a end")
    , (False, "let type intArray = array of string\nvar a := intArray [ord(\"a\")] of chr(1) in a end")
    ]
  expected =
    [ const $ Just $ ExpTy EUnit $ TArray TInt UniqueIgnore
    , const Nothing
    , const Nothing
    , const $ Just $ ExpTy EUnit $ TArray TInt UniqueIgnore
    , const $ Just $ ExpTy EUnit $ TArray TString UniqueIgnore
    ]

testOps :: IO ()
testOps = testTable tests expected
 where
  tests =
    [ (False, "1 + 2")
    , (False, "1 + \"hello\"")
    , (False, "\"hello\" + \"world\"")
    , (False, "\"hello\" & \"world\"")
    , (False, "1 & 1")
    , (False, "\"hello\" < \"world\"")
    , (False, "nil = nil")
    , (False, "nil <= nil")
    , (False, "let type arr = array of int in (arr [10] of 0) = (arr [20] of 1) end")
    , (False, "let type rec={a:int,b:string} in rec{a=1,b=\"a\"} = rec{a=2,b=\"b\"} end")
    , (False, "let type rec1={a:int}\ntype rec2={b:string} in rec1{a=1}=rec2{b=\"a\"} end")
    , (False, "flush() = flush()")
    ]
  expected =
    [ const $ Just $ ExpTy EUnit TInt
    , const Nothing
    , const Nothing
    , const Nothing
    , const $ Just $ ExpTy EUnit TInt
    , const $ Just $ ExpTy EUnit TInt
    , const $ Just $ ExpTy EUnit TInt
    , const Nothing
    , const $ Just $ ExpTy EUnit TInt
    , const $ Just $ ExpTy EUnit TInt
    , const Nothing
    , const Nothing
    ]

testAssign :: IO ()
testAssign = testTable tests expected
 where
  tests =
    [ (False, "let var i := 0 in i := i + 1 end")
    , (False, "let type a = array of int\nvar ar := a[10] of 0 in ar[0] := 1 end")
    , (False, "let var i := 0 in i[0] := 1 end")
    , (False, "let type r = {r:int}\nvar v := r{r=1} in v.r := 2 end")
    , (False, "let var i := 0 in i.foo := 1 end")
    , (False, "let type r={r:int}\ntype a=array of r\nvar ar := a[10] of r{r=1} in ar[0].r := 2 end")
    , (False, "let type a = array of int\n var ar := a[10] of 0 in ar[0] := \"hello\" end")
    , (False, "let type r = {r:int}\nvar v := r{r=1} in v.r := \"hello\" end")
    ]
  expected =
    [ const $ Just $ ExpTy EUnit TUnit
    , const $ Just $ ExpTy EUnit TUnit
    , const Nothing
    , const $ Just $ ExpTy EUnit TUnit
    , const Nothing
    , const $ Just $ ExpTy EUnit TUnit
    , const Nothing
    , const Nothing
    ]

testRec :: IO ()
testRec = testTable tests expected
 where
  tests =
    [ (True, "let type a = b\ntype b = c\ntype c = int in flush() end")
    ]
  expected =
    [ const $ Just $ ExpTy EUnit TUnit
    ]
