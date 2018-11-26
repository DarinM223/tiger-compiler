module Chap5.Semant where

import Control.Monad (when)
import Control.Applicative (liftA2)
import Control.Monad.IO.Class
import Control.Monad.Catch
import Chap2.Lexer (runMyParserT)
import Chap3.Parser (parseExpr)
import Chap5.Symbol (Pos, Symbol, fromSymbol, getSymbols, symbolValue)
import Chap5.Table
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Foldable (foldlM, foldl')
import qualified Chap3.AST as AST
import qualified Data.IntMap as IM
import qualified Data.HashMap.Strict as HM

data Exp = EUnit deriving (Show, Eq)

data ExpTy = ExpTy Exp Ty deriving (Show, Eq)

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
  when (cty /= cty') $
    throwM $ Err pos $ "type mismatch: expected: "
                    ++ show ty ++ " got: " ++ show ty'

lookTy :: (MonadThrow m) => Pos -> Symbol -> TEnv -> m Ty
lookTy pos tySym tenv = case look tySym tenv of
  Just ty -> return ty
  _       -> throwM $ Err pos $ "type " ++ show tySym ++ " not found"

transExp :: (MonadIO m, MonadThrow m) => AST.Exp -> TEnv -> VEnv -> m ExpTy
transExp exp tenv venv = case exp of
  AST.NilExp        -> return $ ExpTy EUnit TNil
  AST.StringExp _ _ -> return $ ExpTy EUnit TString
  AST.IntExp _      -> return $ ExpTy EUnit TInt
  AST.VarExp var    -> trVar var venv

  AST.AssignExp pos var exp -> do
    ExpTy _ ty <- transVar var tenv venv
    ExpTy _ ty' <- transExp exp tenv venv
    checkTy pos ty ty'
    return $ ExpTy EUnit TUnit

  AST.IfExp (AST.IfExp' pos test thenExp elseExp) -> do
    ExpTy _ testTy <- transExp test tenv venv
    ExpTy _ thenTy <- transExp thenExp tenv venv
    elseTy <- mapM
      (\exp -> (\(ExpTy _ ty) -> ty) <$> transExp exp tenv venv)
      elseExp
    checkTy pos TInt testTy
    checkTy pos (fromMaybe TUnit elseTy) thenTy
    return $ ExpTy EUnit thenTy

  AST.WhileExp (AST.WhileExp' pos test body) -> do
    ExpTy _ testTy <- transExp test tenv venv
    ExpTy _ bodyTy <- transExp body tenv venv
    checkTy pos TInt testTy
    checkTy pos TUnit bodyTy
    return $ ExpTy EUnit bodyTy

  AST.ForExp (AST.ForExp' pos varSym _ lo hi body) -> do
    ExpTy _ loTy <- transExp lo tenv venv
    ExpTy _ hiTy <- transExp hi tenv venv
    checkTy pos TInt loTy
    checkTy pos TInt hiTy
    let venv' = enter varSym (VarEntry TInt) venv
    ExpTy _ bodyTy <- transExp body tenv venv'
    checkTy pos TUnit bodyTy
    return $ ExpTy EUnit bodyTy

  AST.RecordExp (AST.RecordExp' pos tySym fields) -> do
    ty <- lookTy pos tySym tenv >>= actualTy
    case ty of
      TRecord fieldTys _ -> do
        fieldTys' <- traverse (trField venv) fields
        matchTys pos fieldTys fieldTys'
        return $ ExpTy EUnit ty
      _ -> throwM $ Err pos $ "type " ++ show tySym ++ " is not a record"

  AST.SeqExp exps -> foldlM
    (\_ (_, exp) -> transExp exp tenv venv)
    (ExpTy EUnit TNil)
    exps

  AST.CallExp (AST.CallExp' pos fnSym args) -> case look fnSym venv of
    Just (FunEntry tys rt) -> do
      tys' <- fmap (fmap (\(ExpTy _ ty) -> ty))
            . traverse (\exp -> transExp exp tenv venv)
            $ args
      if tys == tys'
        then ExpTy EUnit <$> actualTy rt
        else throwM $ Err pos $ "parameters " ++ show tys
                             ++ " don't match with " ++ show tys'
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
  exp -> error $ show exp
 where
  trField venv (pos, symbol, exp) = (\(ExpTy _ ty) -> (pos, symbol, ty))
                                <$> transExp exp tenv venv
  trVar (AST.SimpleVar id pos) venv = case look id venv of
    Just (VarEntry ty) -> ExpTy EUnit <$> actualTy ty
    _ -> throwM $ Err pos $ "undefined variable: " ++ fromSymbol id
  trVar _ _ = undefined

matchTys :: (MonadIO m, MonadThrow m)
         => Pos
         -> [(Symbol, Ty)]
         -> [(Pos, Symbol, Ty)]
         -> m ()
matchTys pos l1 l2
  | length l1 /= length l2 = throwM $ Err pos "Different parameter sizes"
  | otherwise              = checkMap (buildMap l1) l2
 where
  checkMap map l = mapM_ (check map) l
  check map (pos, sym, ty) = case IM.lookup (symbolValue sym) map of
    Just ty' -> checkTy pos ty ty'
    Nothing  -> throwM $
      Err pos $ "Symbol " ++ show sym ++ " not found in record"

  buildMap l = foldl' append IM.empty l
  append map (sym, ty) = IM.insert (symbolValue sym) ty map

transDec :: (MonadIO m, MonadThrow m)
         => AST.Dec -> TEnv -> VEnv -> m (TEnv, VEnv)
transDec dec tenv venv = case dec of
  AST.VarDec (AST.VarDec' _ name Nothing init _) -> do
    ExpTy _ ty <- transExp init tenv venv
    return (tenv, enter name (VarEntry ty) venv)

  AST.VarDec (AST.VarDec' pos name (Just (pos', tySym)) init _) -> do
    ty <- lookTy pos' tySym tenv
    ExpTy _ ty' <- transExp init tenv venv
    checkTy pos ty ty'
    return (tenv, enter name (VarEntry ty) venv)

  AST.TypeDec (AST.TypeDec' _ name ty) ->
    (, venv) <$> (liftA2 (enter name) (transTy ty tenv) (pure tenv))

  AST.FunctionDec (AST.FunDec pos name params Nothing body) ->
    trFunDec pos name params body TUnit

  AST.FunctionDec (AST.FunDec pos name params (Just (pos', rt)) body) ->
    lookTy pos' rt tenv >>= trFunDec pos name params body
 where
  lookTyField (AST.Field pos name tySym _) = (name,) <$> lookTy pos tySym tenv
  trFunDec pos name params body ty = do
    params' <- traverse lookTyField params
    let venv'  = enter name (FunEntry (fmap snd params') ty) venv
        venv'' = foldl' enterParam venv' params'
    ExpTy _ ty' <- transExp body tenv venv''
    checkTy pos ty ty'
    return (tenv, venv')
  enterParam venv (name, ty) = enter name (VarEntry ty) venv

transTy :: (MonadIO m, MonadThrow m) => AST.Ty -> TEnv -> m Ty
transTy (AST.NameTy sym pos) tenv = lookTy pos sym tenv
transTy (AST.RecordTy fields) tenv =
  TRecord <$> trFields fields tenv <*> liftIO mkUnique
 where
  -- TODO(DarinM223): check for duplicate fields in record
  trFields fields tenv = traverse (trField tenv) fields
  trField tenv (AST.Field pos name tySym _) = (name,) <$> lookTy pos tySym tenv
transTy (AST.ArrayTy sym pos) tenv =
  TArray <$> lookTy pos sym tenv <*> liftIO mkUnique

testTy :: String -> IO ExpTy
testTy text = runMyParserT ((,) <$> mkEnvs <*> parseExpr) text >>= \case
  Left err                  -> throwM err
  Right ((tenv, venv), exp) -> transExp exp tenv venv

testTySyms :: String -> IO (HM.HashMap String Int, ExpTy)
testTySyms text = runMyParserT m text >>= \case
  Left err                           -> throwM err
  Right ((tenv, venv), exp, symbols) -> (symbols ,) <$> transExp exp tenv venv
 where
  m = (,,) <$> mkEnvs <*> parseExpr <*> getSymbols
