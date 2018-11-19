module Chap5.Semant
  ( transVar
  , transExp
  , transDec
  , transTy
  ) where

import Control.Monad.IO.Class
import Control.Monad.Catch
import Chap5.Symbol (Pos, fromSymbol)
import Chap5.Table
import Data.IORef
import Data.Foldable (foldlM, foldl')
import qualified Chap3.AST as AST

data Exp = EUnit

data ExpTy = ExpTy Exp Ty

data Err = Err Pos String deriving (Show)
instance Exception Err

transVar :: AST.Var -> TEnv -> VEnv -> m ExpTy
transVar = undefined

-- | Trace through TName types to their underlying definitions.
actualTy :: Ty -> IO Ty
actualTy ty@(TName _ (TRef ref)) = readIORef ref >>= \case
  Just ty -> actualTy ty
  Nothing -> return ty
actualTy ty = return ty

transExp :: (MonadIO m, MonadThrow m) => AST.Exp -> TEnv -> VEnv -> m ExpTy
transExp (AST.VarExp var) _ venv = trVar var venv
transExp (AST.LetExp (AST.LetExp' _ decs body)) tenv venv = do
  (tenv', venv') <- foldlM
    (\(!tenv, !venv) dec -> transDec dec tenv venv)
    (tenv, venv) decs
  transExp body tenv' venv'
transExp (AST.OpExp left AST.PlusOp right pos) tenv venv = do
  ExpTy _ tyleft <- transExp left tenv venv
  ExpTy _ tyright <- transExp right tenv venv
  case (tyleft, tyright) of
    (TInt, TInt) -> return $ ExpTy EUnit TInt
    _            -> throwM $ Err pos "integer required"
transExp _ _ _ = undefined

trVar :: (MonadIO m, MonadThrow m) => AST.Var -> VEnv -> m ExpTy
trVar (AST.SimpleVar id pos) venv = case look id venv of
  Just (VarEntry ty) -> ExpTy EUnit <$> liftIO (actualTy ty)
  _ -> throwM $ Err pos $ "undefined variable: " ++ fromSymbol id
trVar _ _ = undefined

transDec :: (MonadIO m, MonadThrow m)
         => AST.Dec -> TEnv -> VEnv -> m (TEnv, VEnv)
transDec dec tenv venv = case dec of
  AST.VarDec (AST.VarDec' _ name Nothing init _) -> do
    ExpTy _ ty <- transExp init tenv venv
    return (tenv, enter name (VarEntry ty) venv)

  AST.VarDec (AST.VarDec' pos name (Just (pos', tySym)) init _) -> do
    ty <- lookTy pos' tySym
    ExpTy _ ty' <- transExp init tenv venv
    checkTy pos ty ty'
    return (tenv, enter name (VarEntry ty) venv)

  AST.TypeDec (AST.TypeDec' _ name ty) ->
    return (enter name (transTy ty tenv) tenv, venv)

  AST.FunctionDec (AST.FunDec pos name params Nothing body) ->
    trFunDec pos name params body TUnit

  AST.FunctionDec (AST.FunDec pos name params (Just (pos', rt)) body) ->
    lookTy pos' rt >>= trFunDec pos name params body
 where
  lookTyField (AST.Field pos name tySym _) = (name,) <$> lookTy pos tySym
  lookTy pos tySym = case look tySym tenv of
    Just ty -> return ty
    _       -> throwM $ Err pos $ "type " ++ show tySym ++ " not found"
  checkTy pos ty ty'
    | ty == ty' = return ()
    | otherwise = throwM $ Err pos $ "type mismatch: expected: "
               ++ show ty ++ " got: " ++ show ty'
  trFunDec pos name params body ty = do
    params' <- traverse lookTyField params
    let venv'  = enter name (FunEntry (fmap snd params') ty) venv
        venv'' = foldl' enterParam venv' params'
    ExpTy _ ty' <- transExp body tenv venv''
    checkTy pos ty ty'
    return (tenv, venv')
  enterParam venv (name, ty) = enter name (VarEntry ty) venv

transTy :: AST.Ty -> TEnv -> Ty
transTy = undefined
