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
actualTy ty@(TName _ ref) = readIORef ref >>= \case
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
transDec (AST.VarDec (AST.VarDec' { _name = name
                                  , _type = Nothing
                                  , _init = init })) tenv venv = do
  ExpTy _ ty <- transExp init tenv venv
  return (tenv, enter name (VarEntry ty) venv)
transDec (AST.TypeDec (AST.TypeDec' { _name = name, _ty = ty })) tenv venv =
  return (enter name (transTy ty tenv) tenv, venv)
transDec (AST.FunctionDec (AST.FunDec { _name   = name
                                      , _params = params
                                      , _result = Just (pos, rt)
                                      , _body   = body })) tenv venv =
  case look rt tenv of
    Just resultTy -> do
      params' <- traverse lookTy params
      let venv'  = enter name (FunEntry (fmap snd params') resultTy) venv
          venv'' = foldl' enterParam venv' params'
      transExp body tenv venv''
      return (tenv, venv')
    Nothing -> throwM $ Err pos $ "undefined return type"
 where
  lookTy (AST.Field pos name tySym _) = case look tySym tenv of
    Just ty -> return (name, ty)
    _       -> throwM $ Err pos $ "undefined parameter type"
  enterParam venv (name, ty) = enter name (VarEntry ty) venv
transDec _ _ _ = undefined

transTy :: AST.Ty -> TEnv -> Ty
transTy = undefined
