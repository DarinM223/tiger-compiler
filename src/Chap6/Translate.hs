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

-- TODO(DarinM223): might need to pass in TempM into these functions.
unEx :: Exp -> Tree.Exp
unEx (Ex exp) = exp
unEx (Nx stm) = undefined
unEx (Cx f)   = undefined

unNx :: Exp -> Tree.Stm
unNx (Ex exp) = undefined
unNx (Nx stm) = stm
unNx (Cx f)   = undefined

unCx :: Exp -> (Temp.Label -> Temp.Label -> Tree.Stm)
unCx (Ex exp)             = undefined
unCx (Nx stm)             = undefined
unCx (Cx (Conditional f)) = f

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
