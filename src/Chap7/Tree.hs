module Chap7.Tree where

import qualified Chap6.Temp as Temp

data Exp = Const Int
         | Name Temp.Label
         -- ^ Similar to an assembly language label.
         | Temp Temp.Temp
         -- ^ Similar to a register but can have infinite amount.
         | BinOp BinOp Exp Exp
         | Mem Exp
         -- ^ Memory at address.
         --
         -- If on left side of Move, it means "store at address"
         -- If on right side of Move, it means "fetch from address"
         | Call Exp [Exp]
         -- ^ Calls function (first parameter) with parameter list (second
         -- parameter).
         | ESeq Stm Exp
         -- ^ Evaluates the statement, then evaluates and returns the
         -- expression.
         deriving (Show, Eq)

data Stm = Move Exp Exp
         -- ^ MOVE (Temp t, e): Evaluate e and move to temporary t.
         --
         -- MOVE(MEM(e1), e2): Evaluate e1, getting address a.
         -- Then evaluate e2 and store the result in memory at address a.
         | Exp Exp
         -- ^ Evaluate the expression and discard the result.
         | Jump Exp [Temp.Label]
         -- ^ Jumps to address from evaluated expression. Also includes
         -- a list of all possible jump locations for dataflow analysis.
         | CJump RelOp Exp Exp Temp.Label Temp.Label
         -- ^ Evaluate first, then second expression, then compare them
         -- with the relational operator. If the result is true, then jump
         -- to the first label, otherwise jump to the second label.
         | Seq Stm Stm
         | Label Temp.Label
         -- ^ Sets the value of the label to be the current machine code
         -- address.
         deriving (Show, Eq)

data BinOp = Plus | Minus | Mul | Div | And | Or
           | LShift | RShift | ARShift | Xor
           deriving (Show, Eq)

data RelOp = Eq | Ne | Lt | Gt | Le | Ge | Ult | Ule | Ugt | Uge
  deriving (Show, Eq)
