module Chap5.Semant
  ( transVar
  , transExp
  , transDec
  , transTy
  , testTy
  ) where

import Control.Monad (void)
import Control.Monad.IO.Class
import Control.Monad.Catch
import Chap2.Lexer (runMyParserT)
import Chap3.Parser (parseExpr)
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
actualTy :: (MonadIO m) => Ty -> m Ty
actualTy ty@(TName _ (TRef ref)) = liftIO (readIORef ref) >>= \case
  Just ty -> actualTy ty
  Nothing -> return ty
actualTy ty = return ty

checkTy :: (MonadIO m, MonadThrow m) => Pos -> Ty -> Ty -> m ()
checkTy pos ty ty' = do
  cty <- actualTy ty
  cty' <- actualTy ty'
  if cty == cty'
    then return ()
    else throwM $ Err pos $ "type mismatch: expected: "
                         ++ show ty ++ " got: " ++ show ty'

transExp :: (MonadIO m, MonadThrow m) => AST.Exp -> TEnv -> VEnv -> m ExpTy
transExp exp tenv venv = case exp of
  AST.NilExp        -> return $ ExpTy EUnit TNil
  AST.StringExp _ _ -> return $ ExpTy EUnit TString
  AST.IntExp _      -> return $ ExpTy EUnit TInt
  AST.VarExp var    -> trVar var venv

  -- TODO(DarinM223): match types for (Pos, Symbol, Exp) fields
  -- if they typecheck, then return return type
  AST.RecordExp (AST.RecordExp' _ _ _) -> undefined

  AST.CallExp (AST.CallExp' pos fnSym args) -> case look fnSym venv of
    Just (FunEntry tys rt) -> do
      tys' <- fmap (fmap (\(ExpTy _ ty) -> ty))
            . traverse (\exp -> transExp exp tenv venv)
            $ args
      -- TODO(DarinM223): check with checkTy
      if tys == tys'
        then ExpTy EUnit <$> actualTy rt
        -- TODO(DarinM223): make error easier to understand.
        else throwM $ Err pos "parameters don't match"
    _ -> throwM $ Err pos "function not found"

  AST.LetExp (AST.LetExp' _ decs body) -> do
    (tenv', venv') <- foldlM
      (\(!tenv, !venv) dec -> transDec dec tenv venv)
      (tenv, venv) decs
    transExp body tenv' venv'

  AST.OpExp left AST.PlusOp right pos -> do
    ExpTy _ tyleft <- transExp left tenv venv
    ExpTy _ tyright <- transExp right tenv venv
    case (tyleft, tyright) of
      (TInt, TInt) -> return $ ExpTy EUnit TInt
      _            -> throwM $ Err pos "integer required"

  AST.OpExp _ _ _ _ -> undefined
  _ -> undefined
 where
  trVar (AST.SimpleVar id pos) venv = case look id venv of
    Just (VarEntry ty) -> ExpTy EUnit <$> actualTy ty
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

testTy :: String -> IO ()
testTy text = runMyParserT parseExpr text >>= \case
  Left err  -> print err
  Right exp -> void $ transExp exp empty empty
