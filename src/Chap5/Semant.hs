module Chap5.Semant where

import Chap5.Table
import qualified Chap3.AST as AST

data Exp

data ExpTy = ExpTy Exp Ty

transVar :: AST.Var -> TEnv -> VEnv -> ExpTy
transVar = undefined

transExp :: AST.Exp -> TEnv -> VEnv -> ExpTy
transExp = undefined

transDec :: AST.Dec -> TEnv -> VEnv -> (TEnv, VEnv)
transDec = undefined

transTy :: AST.Ty -> TEnv -> Ty
transTy = undefined
