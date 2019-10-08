{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
module ParserTest (tests) where

import Chap2.Lexer
import Chap3.AST
import Control.Monad (when)
import NeatInterpolation
import ST
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec (errorBundlePretty)
import qualified Data.Text as T

tests :: TestTree
tests = testGroup "Parser tests"
  [ testCase "Tests parsing call" (testExp False testParseCall)
  , testCase "Tests on sample program 1" (testExp False testSample1)
  , testCase "Tests on sample program 2" (testExp False testSample2)
  , testCase "Tests empty record dec" (testExp False testParseEmptyRecord)
  ]

testExp :: Bool -> Either ParseErr (Exp BoolValue) -> IO ()
testExp debug = \case
  Left e    -> assertFailure $ errorBundlePretty e
  Right exp -> when debug $ print exp

testParseCall :: Either ParseErr (Exp BoolValue)
testParseCall = parse "foo(hello.bar, blah[boo], hello.world[bob])"

testParseEmptyRecord :: Either ParseErr (Exp BoolValue)
testParseEmptyRecord = parse "let type rec = {} in end"

testSample1 :: Either ParseErr (Exp BoolValue)
testSample1 = parse $ T.unpack
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

testSample2 :: Either ParseErr (Exp BoolValue)
testSample2 = parse $ T.unpack
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
