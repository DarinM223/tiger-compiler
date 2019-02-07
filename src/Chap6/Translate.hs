{-# LANGUAGE RecordWildCards #-}

module Chap6.Translate where

import Chap6.Frame (FrameM (..))
import Chap6.Temp (TempM (..))
import GHC.Records
import qualified Chap6.Frame as Frame
import qualified Chap6.Temp as Temp
import qualified Chap7.Tree as Tree

newtype Conditional = Conditional (Temp.Label -> Temp.Label -> Tree.Stm)
instance Show Conditional where
  show _ = ""
instance Eq Conditional where
  _ == _ = True

data Exp = EUnit -- TODO(DarinM223): remove this later
         | Ex Tree.Exp
         | Nx Tree.Stm
         | Cx Conditional
         -- ^ Conditional: Given a label for the true destination
         -- and a label for the false destination, yields a statement
         -- that evaluates a conditional and jumps to one of the labels.
         deriving (Show, Eq)

seqList :: [Tree.Stm] -> Tree.Stm
seqList [stm]      = stm
seqList (stm:stms) = Tree.Seq stm (seqList stms)
seqList _          = error "Empty list"

unEx :: Monad m => TempM m -> Exp -> m Tree.Exp
unEx TempM{..} = \case
  Ex exp                  -> return exp
  Nx stm                  -> return $ Tree.ESeq stm (Tree.Const 0)
  Cx (Conditional genStm) -> do
    r <- newTemp
    t <- newLabel
    f <- newLabel
    let stm = seqList
          [ Tree.Move (Tree.Temp r) (Tree.Const 1)
          , genStm t f
          , Tree.Label f
          , Tree.Move (Tree.Temp r) (Tree.Const 0)
          , Tree.Label t
          ]
    return $ Tree.ESeq stm (Tree.Temp r)

unNx :: Monad m => TempM m -> Exp -> m Tree.Stm
unNx tempM = \case
  Ex exp   -> return $ Tree.Exp exp
  Nx stm   -> return stm
  c@(Cx _) -> Tree.Exp <$> unEx tempM c

unCx :: Exp -> (Temp.Label -> Temp.Label -> Tree.Stm)
unCx = \case
  Nx _               -> error "This cannot happen"
  Cx (Conditional f) -> f
  Ex (Tree.Const 0)  -> \_ f -> Tree.Label f
  Ex (Tree.Const 1)  -> \t _ -> Tree.Label t
  Ex exp             -> \t f -> Tree.CJump Tree.Eq exp (Tree.Const 1) t f

data Init level = Init
  { _parent  :: level
  , _name    :: Temp.Label
  , _formals :: [Bool]
  }

-- | `access` is not the same as `access` in Frame. It is usually a wrapper
-- around it that also includes information about the level.
data TranslateM level access exp m = TranslateM
  { newLevel     :: Init level -> m level
  , allocLocal   :: level -> Bool -> m access
  , outermost    :: level
  , levelFormals :: level -> [access]
  , simpleVar    :: access -> level -> m exp
  }

data Level frame = Level
  { _parent  :: Level frame
  , _name    :: Temp.Label
  , _formals :: [Bool]
  , _frame   :: frame
  } | Outermost

data Access frame access = Access
  { _level  :: Level frame
  , _access :: access -- ^ Frame access
  }

type TransM frame access m =
  TranslateM (Level frame) (Access frame access) Exp m

mkTranslateM :: Monad m => TempM m -> FrameM access frame m
             -> TranslateM (Level frame) (Access frame access) Exp m
mkTranslateM tempM frameM@FrameM{..} = TranslateM
  { newLevel     = newLevel' tempM frameM
  , allocLocal   = allocLocal' frameM
  , outermost    = Outermost
  , levelFormals = levelFormals'
  , simpleVar    = undefined
  }
 where
  levelFormals' Outermost = []
  levelFormals' level =
    fmap (Access level) . tail . formals . _frame $ level

newLevel' :: Monad m => TempM m -> FrameM access frame m
          -> Init (Level frame) -> m (Level frame)
newLevel' TempM{..} FrameM{..} init = do
  label <- newLabel
  frame <- newFrame (Frame.Init label (True:getField @"_formals" init))
  return Level
    { _parent  = getField @"_parent" init
    , _name    = getField @"_name" init
    , _formals = getField @"_formals" init
    , _frame   = frame
    }

allocLocal' :: Functor m => FrameM access frame m
            -> Level frame -> Bool -> m (Access frame access)
allocLocal' FrameM{..} level escapes =
  Access level <$> allocLocal (_frame level) escapes
