{
module Chap2.Lexer where
}

%wrapper "basic" -- TODO(DarinM223): figure out which wrapper to use.

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

  $white+                       ;
  "--".*                        ;
  let                           { \_ -> Let }
  in                            { \_ -> In }
  $digit+                       { \s -> Int (read s) }
  [\=\+\-\*\/\(\)]              { \s -> Sym (head s) }
  $alpha [$alpha $digit \_ \']* { \s -> Var s }

{

data Token = Let
           | In
           | Sym Char
           | Var String
           | Int Int
           deriving (Eq, Show)

}
