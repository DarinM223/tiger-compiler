{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TypeCheckTest (tests) where

import Control.Monad.Catch
import Chap2.Ref (Ref)
import Chap5.Semant
import Chap5.Symbol
import Chap5.Table hiding (look)
import Chap6.Translate
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.HashMap.Strict as HM

type ExpTyIO = ExpTy (Ref IO)

tests :: TestTree
tests = testGroup "Type checking tests"
  [ testCase "Tests let expression" $
    testExpTy testLet (Just (ExpTy EUnit TInt))
  , testCase "Tests call expression" $
    testExpTy testCall (Just (ExpTy EUnit TInt))
  , testRecord, testIfElse, testWhile, testFor, testArray
  , testOps, testAssign, testRec, testBreak
  ]

eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe (Left _)  = Nothing
eitherToMaybe (Right v) = Just v

testExpTy :: (Show a, Eq a) => IO (Either SomeException a) -> Maybe a -> IO ()
testExpTy m comp = do
  result <- m
  let result' = eitherToMaybe result
  result' == comp @? "Expected: " ++ show comp ++ "\nActual: " ++ show result

testLet :: IO (Either SomeException ExpTyIO)
testLet = try $ testTy "let type a = int\n var a : a := 2 in a end"

testCall :: IO (Either SomeException ExpTyIO)
testCall = try $ testTy $ "let function add(a : int, b: int): int = a + b\n"
                       ++ "var a : int := 2\n var b := 3 in add(a, b) end"

testTableVals :: [String] -> [Maybe ExpTyIO] -> [TestTree]
testTableVals ts es = fmap toTestCase $ zip ([1..] :: [Int]) $ zip ts es
 where toTestCase (n, (t, e)) = testCase (show n) (testExpTy (try (testTy t)) e)

testTableSyms :: [String]
              -> [HM.HashMap String Int -> Maybe ExpTyIO]
              -> [TestTree]
testTableSyms ts es = fmap toTestCase $ zip ([1..] :: [Int]) $ zip ts es
 where
  toTestCase (n, (t, e)) = testCase (show n) runTest
   where
    runTest = try (testTySyms t) >>= \case
      Left (err :: SomeException) -> testExpTy (pure (Left err)) (e HM.empty)
      Right (table, v)            -> testExpTy (pure (Right v)) (e table)

-- | Looks up a symbol from the table.
look :: String -> HM.HashMap String Int -> Symbol
look s table = Symbol (s, table HM.! s)

testRecord :: TestTree
testRecord = testGroup "Test record" $ testTableSyms
  [ "let type rec = { a: int, b: string } in rec { a = 1, b = \"hello\" } end"
  , "let type rec = { a: int, b: string } in rec { a = 1, b = \"hello\", b = \"world\" } end"
  , "let type rec = { a: int, b: string } in rec { a = 1, c = \"hello\" } end"
  , "let type rec = { c: int, d: string } in rec { d = \"hello\", c = 1 } end"
  , "let type rec = { c: int } in rec { } end"
  , "let type rec = { } in rec { } end"
  , "let type rec = { a: int, a: string } in flush() end"
  ]
  [ \t -> Just $ ExpTy EUnit $
    TRecord [(look "a" t, TInt), (look "b" t, TString)] UniqueIgnore
  , const Nothing
  , const Nothing
  , \t -> Just $ ExpTy EUnit $
    TRecord [(look "c" t, TInt), (look "d" t, TString)] UniqueIgnore
  , const Nothing
  , const $ Just $ ExpTy EUnit $ TRecord [] UniqueIgnore
  , const Nothing
  ]

testIfElse :: TestTree
testIfElse = testGroup "Test if else" $ testTableVals
  [ "if 1 then flush()"
  , "if 1 then flush() else 3"
  , "if 1 then 3"
  , "if \"hello\" then flush()"
  , "let var s := \"h\" in if ord(s) then flush() end"
  , "let var s := \"hello\" in if 1 then size(s) else ord(s) end"
  , "let type mi = int\nvar i : mi := 2 in if i then i else 0 end"
  ]
  [ Just $ ExpTy EUnit TUnit
  , Nothing
  , Nothing
  , Nothing
  , Just $ ExpTy EUnit TUnit
  , Just $ ExpTy EUnit TInt
  , Just $ ExpTy EUnit TInt
  ]

testWhile :: TestTree
testWhile = testGroup "Test while" $ testTableVals
  ["while 1 do flush()", "while \"hello\" do flush()", "while 1 do 1"]
  [Just $ ExpTy EUnit TUnit, Nothing, Nothing]

testFor :: TestTree
testFor = testGroup "Test for" $ testTableVals
  [ "for i := 0 to 10 do flush()"
  , "for i := 0 to 10 do print(chr(i))"
  , "for i := 0 to 10 do print(i)"
  , "let var i := \"hello\"\n in for i := 0 to 10 do print(chr(i)) end"
  , "for i := 0 to \"hello\" do flush()"
  , "for i := \"hello\" to 10 do flush()"
  , "for i := 0 to 10 do 1"
  , "for i := ord(\"a\") to ord(\"b\") do flush()"
  ]
  [ Just $ ExpTy EUnit TUnit
  , Just $ ExpTy EUnit TUnit
  , Nothing
  , Just $ ExpTy EUnit TUnit
  , Nothing
  , Nothing
  , Nothing
  , Just $ ExpTy EUnit TUnit
  ]

testArray :: TestTree
testArray = testGroup "Test array" $ testTableVals
  [ "let type intArray = array of int\nvar a := intArray [10] of 0 in a end"
  , "let type intArray = array of int\nvar a := intArray [\"1\"] of 0 in a end"
  , "let type intArray = int\nvar a := intArray [10] of 0 in a end"
  , "let type ia = array of int\ntype ia2 = ia\nvar a := ia2 [10] of 0 in a end"
  , "let type intArray = array of string\nvar a := intArray [ord(\"a\")] of chr(1) in a end"
  ]
  [ Just $ ExpTy EUnit $ TArray TInt UniqueIgnore
  , Nothing
  , Nothing
  , Just $ ExpTy EUnit $ TArray TInt UniqueIgnore
  , Just $ ExpTy EUnit $ TArray TString UniqueIgnore
  ]

testOps :: TestTree
testOps = testGroup "Test operators" $ testTableVals
  [ "1 + 2"
  , "1 + \"hello\""
  , "\"hello\" + \"world\""
  , "\"hello\" & \"world\""
  , "1 & 1"
  , "\"hello\" < \"world\""
  , "nil = nil"
  , "nil <= nil"
  , "let type arr = array of int in (arr [10] of 0) = (arr [20] of 1) end"
  , "let type rec={a:int,b:string} in rec{a=1,b=\"a\"} = rec{a=2,b=\"b\"} end"
  , "let type rec1={a:int}\ntype rec2={b:string} in rec1{a=1}=rec2{b=\"a\"} end"
  , "flush() = flush()"
  ]
  [ Just $ ExpTy EUnit TInt
  , Nothing
  , Nothing
  , Nothing
  , Just $ ExpTy EUnit TInt
  , Just $ ExpTy EUnit TInt
  , Just $ ExpTy EUnit TInt
  , Nothing
  , Just $ ExpTy EUnit TInt
  , Just $ ExpTy EUnit TInt
  , Nothing
  , Nothing
  ]

testAssign :: TestTree
testAssign = testGroup "Test assignment" $ testTableVals
  [ "let var i := 0 in i := i + 1 end"
  , "let type a = array of int\nvar ar := a[10] of 0 in ar[0] := 1 end"
  , "let var i := 0 in i[0] := 1 end"
  , "let type r = {r:int}\nvar v := r{r=1} in v.r := 2 end"
  , "let var i := 0 in i.foo := 1 end"
  , "let type r={r:int}\ntype a=array of r\nvar ar := a[10] of r{r=1} in ar[0].r := 2 end"
  , "let type a = array of int\n var ar := a[10] of 0 in ar[0] := \"hello\" end"
  , "let type r = {r:int}\nvar v := r{r=1} in v.r := \"hello\" end"
  ]
  [ Just $ ExpTy EUnit TUnit
  , Just $ ExpTy EUnit TUnit
  , Nothing
  , Just $ ExpTy EUnit TUnit
  , Nothing
  , Just $ ExpTy EUnit TUnit
  , Nothing
  , Nothing
  ]

testRec :: TestTree
testRec = testGroup "Test recursive declarations" $ testTableVals
  [ "let type a = b\ntype b = c\ntype c = int\nvar a : a := 2 in a end"
  , "let type a = b\ntype b = c\ntype c = a\nvar a : a := 2 in a end"
  , "let type a = { next : a } in flush() end"
  , "let type a = array of a in flush() end"
  , "let type a = int\ntype a = int in flush() end"
  , "let function add(a : int, b : int) : int = add(a - 1, b - 1) in add(1, 2) end"
  , "let function a(i:int):int = b(i)\nfunction b(i:int):int = a(i) in a(1) end"
  , "let function a(i:int):int = b(i)\nfunction b(i:int):string = \"a\" in a(1) end"
  , "let function a(i:int):int = 2\nfunction a(i:string):int = 2 in flush() end"
  , "let function a(i:int, i:string):int = 2 in flush() end"
  ]
  [ Just $ ExpTy EUnit TInt
  , Nothing
  , Just $ ExpTy EUnit TUnit
  , Just $ ExpTy EUnit TUnit
  , Nothing
  , Just $ ExpTy EUnit TInt
  , Just $ ExpTy EUnit TInt
  , Nothing
  , Nothing
  , Nothing
  ]

testBreak :: TestTree
testBreak = testGroup "Test break" $ testTableVals
  [ "break"
  , "while 1 do break"
  , "for i := 0 to 10 do break"
  , "while 1 do let function foo(i:int)=(print(\"a\");break) in foo(1) end"
  , "for i := 0 to 10 do if 1 then flush() else break"
  , "for i := 0 to 10 do (break; while 1 do break)"
  ]
  [ Nothing
  , Just $ ExpTy EUnit TUnit
  , Just $ ExpTy EUnit TUnit
  , Just $ ExpTy EUnit TUnit
  , Just $ ExpTy EUnit TUnit
  , Just $ ExpTy EUnit TUnit
  ]
