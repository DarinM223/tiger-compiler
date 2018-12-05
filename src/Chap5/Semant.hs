module Chap5.Semant where

import Control.Monad (forM_, void, when)
import Control.Monad.IO.Class
import Control.Monad.Catch
import Chap2.Lexer (runMyParserT)
import Chap3.Parser (parseExpr)
import Chap5.Symbol (Pos, Symbol (Symbol), fromSymbol, getSymbols, symbolValue)
import Chap5.Table
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Foldable (foldlM, foldl')
import qualified Chap3.AST as AST
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.HashMap.Strict as HM

data Exp = EUnit deriving (Show, Eq)
data ExpTy = ExpTy Exp Ty deriving (Show, Eq)
data OpType = Arithmetic -- ^ Operation over two integers
            | Comparison -- ^ Operation over integers or strings
            | Equality   -- ^ Operations over any two equal types
            deriving (Show, Eq)

data Err = Err Pos String deriving (Show)
instance Exception Err

transVar :: (MonadIO m, MonadThrow m)
         => AST.Var -> TEnv -> VEnv -> Bool -> m ExpTy
transVar var tenv venv break = case var of
  AST.SimpleVar id pos -> case look id venv of
    Just (VarEntry ty) -> ExpTy EUnit <$> actualTy ty
    _ -> throwM $ Err pos $ "undefined variable: " ++ fromSymbol id
  AST.FieldVar var' id pos -> do
    ty <- transVar var' tenv venv break >>= \(ExpTy _ var) -> actualTy var
    case ty of
      TRecord fields _ -> case lookup id fields of
        Just ty -> ExpTy EUnit <$> actualTy ty
        Nothing -> throwM $ Err pos $ "field " ++ show id ++ " not in record"
      _ -> throwM $ Err pos $ "lvalue not a record type"
  AST.SubscriptVar var' exp pos -> do
    ty <- transVar var' tenv venv break >>= \(ExpTy _ var) -> actualTy var
    ExpTy _ expTy <- transExp exp tenv venv break
    checkTy pos TInt expTy
    case ty of
      TArray ty _ -> return $ ExpTy EUnit ty
      _           -> throwM $ Err pos $ "lvalue not an array type"

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

checkDup :: (MonadIO m, MonadThrow m) => [(Symbol, Pos)] -> m ()
checkDup = void . foldlM go IS.empty
 where
  go set (Symbol (name, id), pos)
    | IS.member id set = throwM $ Err pos $ "Duplicate symbol: " ++ name
    | otherwise        = return $ IS.insert id set

lookTy :: (MonadThrow m) => Pos -> Symbol -> TEnv -> m Ty
lookTy pos tySym tenv = case look tySym tenv of
  Just ty -> return ty
  _       -> throwM $ Err pos $ "type " ++ show tySym ++ " not found"

opTypeFromOp :: AST.Op -> OpType
opTypeFromOp = \case
  AST.PlusOp   -> Arithmetic
  AST.MinusOp  -> Arithmetic
  AST.TimesOp  -> Arithmetic
  AST.DivideOp -> Arithmetic
  AST.EqOp     -> Equality
  AST.NeqOp    -> Equality
  AST.AndOp    -> Arithmetic
  AST.OrOp     -> Arithmetic
  AST.LtOp     -> Comparison
  AST.LeOp     -> Comparison
  AST.GtOp     -> Comparison
  AST.GeOp     -> Comparison

checkOpType :: OpType -> Ty -> Bool
checkOpType Arithmetic ty = ty == TInt
checkOpType Comparison ty = ty == TInt || ty == TString
checkOpType Equality ty   = case ty of
  TInt        -> True
  TString     -> True
  TRecord _ _ -> True
  TArray _ _  -> True
  TNil        -> True
  _           -> False

transExp :: (MonadIO m, MonadThrow m)
         => AST.Exp -> TEnv -> VEnv -> Bool -> m ExpTy
transExp exp tenv venv break = case exp of
  AST.NilExp        -> return $ ExpTy EUnit TNil
  AST.StringExp _ _ -> return $ ExpTy EUnit TString
  AST.IntExp _      -> return $ ExpTy EUnit TInt
  AST.VarExp var    -> transVar var tenv venv break

  AST.BreakExp pos -> if break
    then return $ ExpTy EUnit TUnit
    else throwM $ Err pos $ "Break statement not inside enclosing loop"

  AST.AssignExp pos var exp -> do
    ExpTy _ ty <- transVar var tenv venv break
    ExpTy _ ty' <- transExp exp tenv venv break
    checkTy pos ty ty'
    return $ ExpTy EUnit TUnit

  AST.IfExp (AST.IfExp' pos test thenExp elseExp) -> do
    ExpTy _ testTy <- transExp test tenv venv break
    ExpTy _ thenTy <- transExp thenExp tenv venv break
    elseTy <- traverse
      (\exp -> (\(ExpTy _ ty) -> ty) <$> transExp exp tenv venv break)
      elseExp
    checkTy pos TInt testTy
    checkTy pos (fromMaybe TUnit elseTy) thenTy
    return $ ExpTy EUnit thenTy

  AST.WhileExp (AST.WhileExp' pos test body) -> do
    ExpTy _ testTy <- transExp test tenv venv break
    ExpTy _ bodyTy <- transExp body tenv venv True
    checkTy pos TInt testTy
    checkTy pos TUnit bodyTy
    return $ ExpTy EUnit bodyTy

  AST.ForExp (AST.ForExp' pos varSym _ lo hi body) -> do
    ExpTy _ loTy <- transExp lo tenv venv break
    ExpTy _ hiTy <- transExp hi tenv venv break
    checkTy pos TInt loTy
    checkTy pos TInt hiTy
    let venv' = enter varSym (VarEntry TInt) venv
    ExpTy _ bodyTy <- transExp body tenv venv' True
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

  AST.ArrayExp (AST.ArrayExp' pos tySym size init) -> do
    arrayTy <- lookTy pos tySym tenv >>= actualTy
    case arrayTy of
      TArray ty _ -> do
        ExpTy _ sizeTy <- transExp size tenv venv break
        ExpTy _ initTy <- transExp init tenv venv break
        checkTy pos TInt sizeTy
        checkTy pos ty initTy
        return $ ExpTy EUnit arrayTy
      _ -> throwM $ Err pos $ "type " ++ show tySym ++ " is not an array"

  AST.SeqExp exps -> foldlM
    (\_ (_, exp) -> transExp exp tenv venv break)
    (ExpTy EUnit TNil)
    exps

  AST.CallExp (AST.CallExp' pos fnSym args) -> case look fnSym venv of
    Just (FunEntry tys rt) -> do
      tys' <- fmap (fmap (\(ExpTy _ ty) -> ty))
            . traverse (\exp -> transExp exp tenv venv break)
            $ args
      if tys == tys'
        then ExpTy EUnit <$> actualTy rt
        else throwM $ Err pos $ "parameters " ++ show tys
                             ++ " don't match with " ++ show tys'
    _ -> throwM $ Err pos "function not found"

  AST.LetExp (AST.LetExp' _ decs body) -> do
    (tenv', venv') <- foldlM
      (\(!tenv, !venv) dec -> transDec dec tenv venv break)
      (tenv, venv) decs
    transExp body tenv' venv' break

  AST.OpExp left op right pos -> do
    ExpTy _ tyleft <- transExp left tenv venv break
    ExpTy _ tyright <- transExp right tenv venv break
    let opType = opTypeFromOp op
        check  = if checkOpType opType tyleft
          then checkTy pos tyleft tyright
          else throwM $ Err pos $ "Invalid type " ++ show tyleft
                               ++ " for " ++ show opType
    ExpTy EUnit TInt <$ check
 where
  trField venv (pos, symbol, exp) = (\(ExpTy _ ty) -> (pos, symbol, ty))
                                <$> transExp exp tenv venv break
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
         => AST.Dec -> TEnv -> VEnv -> Bool -> m (TEnv, VEnv)
transDec dec tenv venv break = case dec of
  AST.VarDec (AST.VarDec' _ name Nothing init _) -> do
    ExpTy _ ty <- transExp init tenv venv break
    return (tenv, enter name (VarEntry ty) venv)

  AST.VarDec (AST.VarDec' pos name (Just (pos', tySym)) init _) -> do
    ty <- lookTy pos' tySym tenv
    ExpTy _ ty' <- transExp init tenv venv break
    checkTy pos ty ty'
    return (tenv, enter name (VarEntry ty) venv)

  AST.TypeDec decs -> do
    checkDup $ fmap (\(AST.TypeDec' pos name _) -> (name, pos)) decs
    -- First pass: for each declaration, add a TName with the left side symbol.
    --
    -- Example:
    -- type a = b
    -- type b = c
    --
    -- Result tenv:
    -- a: TName "a" (Ref Nothing)
    -- b: TName "b" (Ref Nothing)
    tenv' <- foldlM
      (\env (AST.TypeDec' _ name _) ->
        enter name <$> (TName <$> pure name <*> mkTRef) <*> pure env)
      tenv
      decs

    -- Second pass: if left side symbol is a TName, then translate
    -- the right side type and set the TName's ref to the translated type.
    --
    -- Example:
    -- type a = b
    -- type b = int
    --
    -- Result tenv:
    -- a: TName "a" (Ref (Just (pointer to b)))
    -- b: TName "b" (Ref (Just (pointer to int)))
    forM_ decs $ \(AST.TypeDec' _ lsym ty) -> case look lsym tenv' of
      Just (TName _ ref) -> writeTRef ref =<< transTy ty tenv'
      _                  -> return ()

    -- Third pass: check declarations for cycles.
    mapM_ (\(AST.TypeDec' pos name _) -> checkCycles pos name tenv') decs
    return (tenv', venv)

  AST.FunctionDec decs -> do
    checkDup $ fmap (\(AST.FunDec pos name _ _ _) -> (name, pos)) decs
    -- First pass: gather the headers of all the functions
    -- (function name, function parameter types, function return type)
    venv' <- foldlM
      (\env (AST.FunDec _ name params rt _) ->
        enter name <$> funHeader tenv params rt <*> pure env)
      venv
      decs

    -- Second pass: for each declaration, insert the parameter
    -- names into temporary environment and typecheck the body with it.
    -- Then check that the return type is the same as the function's.
    forM_ decs $ \(AST.FunDec pos _ params rt body) -> do
      venv'' <- foldlM
        (\env (AST.Field _ name ty _) -> enter name
                                     <$> (VarEntry <$> lookTy pos ty tenv)
                                     <*> pure env)
        venv'
        params
      rty <- maybe (pure TUnit) (\(_, ty) -> lookTy pos ty tenv) rt
      ExpTy _ ty <- transExp body tenv venv'' break
      checkTy pos rty ty
    return (tenv, venv')
 where
  checkCycles pos name tenv = go [] name
   where
    go seen name
      | name `elem` seen = throwM $ Err pos $ "Cyclical dependency found"
      | otherwise = case look name tenv of
        Just(TName _ ref) -> readTRef ref >>= \case
          Just (TName sym _) -> go (name:seen) sym
          _                  -> return ()
        _ -> return ()
  funHeader tenv fields rt = do
    checkDup $ fmap (\(AST.Field pos name _ _) -> (name, pos)) fields
    FunEntry
      <$> traverse (\(AST.Field pos _ ty _) -> lookTy pos ty tenv) fields
      <*> maybe (pure TUnit) (\(pos, ty) -> lookTy pos ty tenv) rt

transTy :: (MonadIO m, MonadThrow m) => AST.Ty -> TEnv -> m Ty
transTy (AST.NameTy sym pos) tenv  = lookTy pos sym tenv
transTy (AST.RecordTy fields) tenv = do
  checkDup $ fmap (\(AST.Field pos name _ _) -> (name, pos)) fields
  TRecord <$> trFields fields tenv <*> mkUnique
 where
  trFields fields tenv = traverse (trField tenv) fields
  trField tenv (AST.Field pos name tySym _) = (name,) <$> lookTy pos tySym tenv
transTy (AST.ArrayTy sym pos) tenv =
  TArray <$> lookTy pos sym tenv <*> mkUnique

testTy :: String -> IO ExpTy
testTy text = runMyParserT ((,) <$> mkEnvs <*> parseExpr) text >>= \case
  Left err                  -> throwM err
  Right ((tenv, venv), exp) -> transExp exp tenv venv False

testTySyms :: String -> IO (HM.HashMap String Int, ExpTy)
testTySyms text = runMyParserT m text >>= \case
  Left err                           -> throwM err
  Right ((tenv, venv), exp, symbols) ->
    (symbols ,) <$> transExp exp tenv venv False
 where
  m = (,,) <$> mkEnvs <*> parseExpr <*> getSymbols
