module Chap1
  ( maxargs
  , interp
  , runApp
  )
where

import Control.Monad
import Data.Foldable (foldlM)

type Id = String
type Env = [(Id, Int)]

data BinOp = Plus | Minus | Times | Div deriving (Show, Eq)

data Stm = CompoundStm Stm Stm
         | AssignStm Id Exp
         | PrintStm [Exp]
         deriving (Show, Eq)

data Exp = IdExp Id
         | NumExp Int
         | OpExp BinOp Exp Exp
         | EseqExp Stm Exp
         deriving (Show, Eq)

maxargs :: Stm -> Int
maxargs (CompoundStm a b  ) = max (maxargs a) (maxargs b)
maxargs (AssignStm   _ exp) = maxargsExp exp
maxargs (PrintStm l       ) = max (length l) (maxInList l)
 where
  maxInList [] = 0
  maxInList l  = maximum $ fmap maxargsExp l

maxargsExp :: Exp -> Int
maxargsExp (EseqExp stm exp   ) = max (maxargs stm) (maxargsExp exp)
maxargsExp (OpExp _ left right) = max (maxargsExp left) (maxargsExp right)
maxargsExp _                    = 0

evalStmt :: Env -> Stm -> IO Env
evalStmt bindings (CompoundStm a b) = do
  bindings' <- evalStmt bindings a
  evalStmt bindings' b
evalStmt bindings (AssignStm id exp) = do
  (value, bindings') <- evalExp bindings exp
  return $ (id, value) : bindings'
evalStmt bindings (PrintStm exps) = foldlM evalAndPrint bindings exps
 where
  evalAndPrint env exp = evalExp env exp >>= printExp
  printExp (value, bindings) = print value >> return bindings

evalExp :: Env -> Exp -> IO (Int, Env)
evalExp bindings (IdExp id) = return (lookupValue id bindings, bindings)
evalExp bindings (NumExp num) = return (num, bindings)
evalExp bindings (OpExp op left right) = do
  (leftValue , bindings' ) <- evalExp bindings left
  (rightValue, bindings'') <- evalExp bindings' right
  let
    result = case op of
      Plus  -> leftValue + rightValue
      Minus -> leftValue - rightValue
      Times -> leftValue * rightValue
      Div   -> leftValue `quot` rightValue
  return (result, bindings'')
evalExp bindings (EseqExp stm exp) = evalStmt bindings stm >>= flip evalExp exp

lookupValue :: Id -> Env -> Int
lookupValue id ((id', num) : _) | id == id' = num
lookupValue id (_ : rest)                   = lookupValue id rest
lookupValue _  []                           = error "Id not in environment"

interp :: Stm -> IO ()
interp = void . evalStmt []

runApp :: IO ()
runApp = do
  let
    prog = CompoundStm
      (AssignStm "a" (OpExp Plus (NumExp 5) (NumExp 3)))
      (CompoundStm
        (AssignStm
          "b"
          (EseqExp
            (PrintStm [IdExp "a", OpExp Minus (IdExp "a") (NumExp 1)])
            (OpExp Times (NumExp 10) (IdExp "a"))
          )
        )
        (PrintStm [IdExp "b"])
      )
  putStrLn $ "Max args: " ++ show (maxargs prog)
  putStrLn "Starting interpretation:"
  interp prog
