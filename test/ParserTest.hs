{-# LANGUAGE QuasiQuotes #-}

module ParserTest (tests) where

import Chap2.Lexer
import Chap3.AST
import Chap3.Parser
import NeatInterpolation
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec
import qualified Data.Text as T

tests :: TestTree
tests = testGroup "Parser tests"
  [ testCase "Tests parsing call" (testExp testParseCall False)
  , testCase "Tests on sample program 1" (testExp testSample1 False)
  , testCase "Tests on sample program 2" (testExp testSample2 False)
  , testCase "Tests empty record dec" (testExp testParseEmptyRecord False)
  ]

testExp :: IO (Either ParseErr Exp) -> Bool -> IO ()
testExp m debug = m >>= \case
  Left e    -> assertFailure $ errorBundlePretty e
  Right exp -> if debug then print exp else return ()

testParseCall :: IO (Either ParseErr Exp)
testParseCall = runMyParserT
  parseExpr
  "foo(hello.bar, blah[boo], hello.world[bob])"

testParseEmptyRecord :: IO (Either ParseErr Exp)
testParseEmptyRecord = runMyParserT parseExpr "let type rec = {} in end"

testSample1 :: IO (Either ParseErr Exp)
testSample1 = runMyParserT parseExpr sample1
 where
  sample1 = T.unpack
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

testSample2 :: IO (Either ParseErr Exp)
testSample2 = runMyParserT parseExpr sample2
 where
  sample2 = T.unpack
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
