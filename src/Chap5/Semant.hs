{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Chap5.Semant
  ( ExpTy (..)
  , runExp
  , testTy
  , testTySyms
  ) where

import Control.Monad (forM_, void, when)
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Monad.Reader
import Chap2.Lexer (Config (..), mkConfig)
import Chap3.Parser (parseExpr)
import Chap5.Symbol
import Chap5.Table
import Chap6.Temp (mkTempM, TempM (TempM, namedLabel))
import Chap6.Translate
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Foldable (foldlM, foldl')
import Text.Megaparsec (runParserT)
import qualified Chap3.AST as AST
import qualified Chap6.MipsFrame as Mips
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.HashMap.Strict as HM

data ExpTy = ExpTy Exp Ty deriving (Show, Eq)
data OpType = Arithmetic -- ^ Operation over two integers
            | Comparison -- ^ Operation over integers or strings
            | Equality   -- ^ Operations over any two equal types
            deriving (Show, Eq)

data Err = Err Pos String deriving (Show)
instance Exception Err

data Ctx frame access = Ctx
  { gLevel :: Level frame
  , gTenv  :: TEnv
  , gVenv  :: VEnv frame access
  , gBreak :: Bool
  }

transVar :: (MonadIO m, MonadThrow m)
         => ( ?symbolM :: SymbolM m
            , ?transM :: TransM frame access m
            , ?ctx :: Ctx frame access )
         => AST.Var -> m ExpTy
transVar = \case
  AST.SimpleVar id pos -> case look id gVenv of
    Just (VarEntry _ ty) -> ExpTy EUnit <$> actualTy ty
    _ -> throwM $ Err pos $ "undefined variable: " ++ fromSymbol id
  AST.FieldVar var' id pos -> do
    ty <- transVar var' >>= \(ExpTy _ var) -> actualTy var
    case ty of
      TRecord fields _ -> case lookup id fields of
        Just ty -> ExpTy EUnit <$> actualTy ty
        Nothing -> throwM $ Err pos $ "field " ++ show id ++ " not in record"
      _ -> throwM $ Err pos "lvalue not a record type"
  AST.SubscriptVar var' exp pos -> do
    ty <- transVar var' >>= \(ExpTy _ var) -> actualTy var
    ExpTy _ expTy <- transExp exp
    checkTy pos TInt expTy
    case ty of
      TArray ty _ -> return $ ExpTy EUnit ty
      _           -> throwM $ Err pos "lvalue not an array type"
 where Ctx{..} = ?ctx

-- | Trace through TName types to their underlying definitions.
actualTy :: MonadIO m => Ty -> m Ty
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

lookTy :: MonadThrow m => Pos -> Symbol -> TEnv -> m Ty
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
         => ( ?symbolM :: SymbolM m
            , ?transM :: TransM frame access m
            , ?ctx :: Ctx frame access )
         => AST.Exp -> m ExpTy
transExp = \case
  AST.NilExp        -> return $ ExpTy EUnit TNil
  AST.StringExp _ _ -> return $ ExpTy EUnit TString
  AST.IntExp _      -> return $ ExpTy EUnit TInt
  AST.VarExp var    -> transVar var

  AST.BreakExp pos -> if gBreak
    then return $ ExpTy EUnit TUnit
    else throwM $ Err pos "Break statement not inside enclosing loop"

  AST.AssignExp pos var exp -> do
    ExpTy _ ty <- transVar var
    ExpTy _ ty' <- transExp exp
    checkTy pos ty ty'
    return $ ExpTy EUnit TUnit

  AST.IfExp (AST.IfExp' pos test thenExp elseExp) -> do
    ExpTy _ testTy <- transExp test
    ExpTy _ thenTy <- transExp thenExp
    elseTy <- traverse (fmap (\(ExpTy _ ty) -> ty) . transExp) elseExp
    checkTy pos TInt testTy
    checkTy pos (fromMaybe TUnit elseTy) thenTy
    return $ ExpTy EUnit thenTy

  AST.WhileExp (AST.WhileExp' pos test body) -> do
    ExpTy _ testTy <- transExp test
    ExpTy _ bodyTy <- let ?ctx = ?ctx { gBreak = True }
                      in transExp body
    checkTy pos TInt testTy
    checkTy pos TUnit bodyTy
    return $ ExpTy EUnit bodyTy

  AST.ForExp (AST.ForExp' pos varSym escRef lo hi body) -> do
    limitSym <- toSymbol "limit"
    let ivar     = AST.SimpleVar varSym pos
        limitvar = AST.SimpleVar limitSym pos
    limitEscRef <- AST.mkEscape
    -- FIXME(DarinM223): wraps body in if block to check that type is TUnit.
    let decs =
          [ AST.VarDec $ AST.VarDec' pos varSym Nothing lo escRef
          , AST.VarDec $ AST.VarDec' pos limitSym Nothing hi limitEscRef
          ]
        test   = AST.OpExp (AST.VarExp ivar) AST.LeOp (AST.VarExp limitvar) pos
        incr   = AST.OpExp (AST.VarExp ivar) AST.PlusOp (AST.IntExp 1) pos
        body'  = AST.IfExp $ AST.IfExp' pos (AST.IntExp 1) body Nothing
        body'' = AST.SeqExp [(pos, body'), (pos, AST.AssignExp pos ivar incr)]
        loop   = AST.WhileExp $ AST.WhileExp' pos test body''
    transExp $ AST.LetExp $ AST.LetExp' pos decs loop

  AST.RecordExp (AST.RecordExp' pos tySym fields) -> do
    ty <- lookTy pos tySym gTenv >>= actualTy
    case ty of
      TRecord fieldTys _ -> do
        fieldTys' <- traverse trField fields
        matchTys pos fieldTys fieldTys'
        return $ ExpTy EUnit ty
      _ -> throwM $ Err pos $ "type " ++ show tySym ++ " is not a record"

  AST.ArrayExp (AST.ArrayExp' pos tySym size init) -> do
    arrayTy <- lookTy pos tySym gTenv >>= actualTy
    case arrayTy of
      TArray ty _ -> do
        ExpTy _ sizeTy <- transExp size
        ExpTy _ initTy <- transExp init
        checkTy pos TInt sizeTy
        checkTy pos ty initTy
        return $ ExpTy EUnit arrayTy
      _ -> throwM $ Err pos $ "type " ++ show tySym ++ " is not an array"

  AST.SeqExp exps ->
    foldlM (\_ (_, exp) -> transExp exp) (ExpTy EUnit TNil) exps

  AST.CallExp (AST.CallExp' pos fnSym args) -> case look fnSym gVenv of
    Just (FunEntry _ _ tys rt) -> do
      tys' <- fmap (\(ExpTy _ ty) -> ty) <$> traverse transExp args
      if tys == tys'
        then ExpTy EUnit <$> actualTy rt
        else throwM $ Err pos $ "parameters " ++ show tys
                             ++ " don't match with " ++ show tys'
    _ -> throwM $ Err pos "function not found"

  AST.LetExp (AST.LetExp' _ decs body) -> do
    (tenv', venv') <- foldlM
      (\(!tenv, !venv) dec -> let ?ctx = ?ctx { gTenv = tenv, gVenv = venv }
                              in transDec dec)
      (gTenv, gVenv) decs
    let ?ctx = ?ctx { gTenv = tenv', gVenv = venv' } in
      transExp body

  AST.OpExp left op right pos -> do
    ExpTy _ tyleft <- transExp left
    ExpTy _ tyright <- transExp right
    let opType = opTypeFromOp op
        check  = if checkOpType opType tyleft
          then checkTy pos tyleft tyright
          else throwM $ Err pos $ "Invalid type " ++ show tyleft
                               ++ " for " ++ show opType
    ExpTy EUnit TInt <$ check
 where
  SymbolM{..} = ?symbolM
  Ctx{..} = ?ctx
  trField (pos, sym, exp) = (\(ExpTy _ ty) -> (pos, sym, ty)) <$> transExp exp

matchTys :: (MonadIO m, MonadThrow m)
         => Pos
         -> [(Symbol, Ty)]
         -> [(Pos, Symbol, Ty)]
         -> m ()
matchTys pos l1 l2
  | length l1 /= length l2 = throwM $ Err pos "Different parameter sizes"
  | otherwise              = checkMap (buildMap l1) l2
 where
  checkMap map = mapM_ (check map)
  check map (pos, sym, ty) = case IM.lookup (symbolValue sym) map of
    Just ty' -> checkTy pos ty ty'
    Nothing  -> throwM $
      Err pos $ "Symbol " ++ show sym ++ " not found in record"

  buildMap = foldl' append IM.empty
  append map (sym, ty) = IM.insert (symbolValue sym) ty map

transDec :: (MonadIO m, MonadThrow m)
         => ( ?symbolM :: SymbolM m
            , ?transM :: TransM frame access m
            , ?ctx :: Ctx frame access )
         => AST.Dec -> m (TEnv, VEnv frame access)
transDec = \case
  AST.VarDec (AST.VarDec' _ name Nothing init escapeRef) -> do
    ExpTy _ ty <- transExp init
    escape <- AST.readEscape escapeRef
    access <- allocLocal gLevel escape
    return (gTenv, enter name (VarEntry access ty) gVenv)

  AST.VarDec (AST.VarDec' pos name (Just (pos', tySym)) init escapeRef) -> do
    ty <- lookTy pos' tySym gTenv
    ExpTy _ ty' <- transExp init
    checkTy pos ty ty'
    escape <- AST.readEscape escapeRef
    access <- allocLocal gLevel escape
    return (gTenv, enter name (VarEntry access ty) gVenv)

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
      gTenv
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
    return (tenv', gVenv)

  AST.FunctionDec decs -> do
    checkDup $ fmap (\(AST.FunDec pos name _ _ _) -> (name, pos)) decs
    -- First pass: gather the headers of all the functions
    -- (function name, function parameter types, function return type)
    venv' <- foldlM
      (\env (AST.FunDec _ name params rt _) ->
        enter name <$> funHeader gTenv name gLevel params rt <*> pure env)
      gVenv
      decs

    -- Second pass: for each declaration, insert the parameter
    -- names into temporary environment and typecheck the body with it.
    -- Then check that the return type is the same as the function's.
    forM_ decs $ \(AST.FunDec pos name params rt body) -> do
      let newlevel = case look name venv' of
            Just (FunEntry newlevel _ _ _) -> newlevel
            _ -> error "Function entry doesn't exist in venv"
      venv'' <- foldlM
        (\env (AST.Field _ name ty _, access)
          ->  enter name
          <$> (VarEntry access <$> lookTy pos ty gTenv)
          <*> pure env)
        venv'
        (zip params (levelFormals newlevel))
      rty <- maybe (pure TUnit) (\(_, ty) -> lookTy pos ty gTenv) rt
      ExpTy _ ty <- let ?ctx = ?ctx { gLevel = newlevel, gVenv = venv'' }
                    in transExp body
      checkTy pos rty ty
    return (gTenv, venv')
 where
  TranslateM{..} = ?transM
  Ctx{..} = ?ctx

  checkCycles pos name tenv = go [] name
   where
    go seen name
      | name `elem` seen = throwM $ Err pos "Cyclical dependency found"
      | otherwise = case look name tenv of
        Just(TName _ ref) -> readTRef ref >>= \case
          Just (TName sym _) -> go (name:seen) sym
          _                  -> return ()
        _ -> return ()
  funHeader tenv name level fields rt = do
    checkDup $ fmap (\(AST.Field pos name _ _) -> (name, pos)) fields
    escapes <- traverse AST.readEscape
             $ fmap (\(AST.Field _ _ _ esc) -> esc) fields
    FunEntry
      <$> newLevel (Init level name escapes)
      <*> pure name
      <*> traverse (\(AST.Field pos _ ty _) -> lookTy pos ty tenv) fields
      <*> maybe (pure TUnit) (\(pos, ty) -> lookTy pos ty tenv) rt

transTy :: (MonadIO m, MonadThrow m) => AST.Ty -> TEnv -> m Ty
transTy (AST.NameTy sym pos) tenv  = lookTy pos sym tenv
transTy (AST.RecordTy fields) tenv = do
  checkDup $ fmap (\(AST.Field pos name _ _) -> (name, pos)) fields
  TRecord <$> trFields fields tenv <*> mkUnique
 where
  trFields fields tenv = traverse (trField tenv) fields
  trField tenv (AST.Field pos name tySym _) = (name,) <$> lookTy pos tySym tenv
transTy (AST.ArrayTy sym pos) tenv = TArray <$> lookTy pos sym tenv <*> mkUnique

runExp :: (MonadIO m, MonadThrow m)
       => SymbolM m
       -> TempM m
       -> TransM Mips.Frame Mips.Access m
       -> TEnv
       -> VEnv Mips.Frame Mips.Access
       -> AST.Exp
       -> m ExpTy
runExp symbolM TempM{..} transM@TranslateM{..} tenv venv exp = do
  mainName <- namedLabel "main"
  mainLevel <- newLevel (Init Outermost mainName [])
  let ?symbolM = symbolM
      ?transM  = transM
      ?ctx     = Ctx { gLevel = mainLevel
                     , gTenv  = tenv
                     , gVenv  = venv
                     , gBreak = False
                     }
  transExp exp

testTy :: String -> IO ExpTy
testTy text = do
  config <- mkConfig
  let symbolM = mkSymbolM (_symRef config) (_symTable config)
      tempM   = mkTempM symbolM (_tempRef config)
      transM  = mkTranslateM tempM (Mips.mkFrameM tempM)
  (tenv, venv) <- mkEnvs symbolM transM
  runReaderT (runParserT parseExpr "" text) config >>= \case
    Left err  -> throwM err
    Right exp -> runExp symbolM tempM transM tenv venv exp

testTySyms :: String -> IO (HM.HashMap String Int, ExpTy)
testTySyms text = do
  config <- mkConfig
  let symbolM = mkSymbolM (_symRef config) (_symTable config)
      tempM   = mkTempM symbolM (_tempRef config)
      transM  = mkTranslateM tempM (Mips.mkFrameM tempM)
  (tenv, venv) <- mkEnvs symbolM transM
  runReaderT (runParserT (parse config) "" text) config >>= \case
    Left err             -> throwM err
    Right (exp, symbols) -> (symbols ,)
                        <$> runExp symbolM tempM transM tenv venv exp
 where
  parse config = (,)
    <$> parseExpr
    <*> getSymbols (mkSymbolM (_symRef config) (_symTable config))
