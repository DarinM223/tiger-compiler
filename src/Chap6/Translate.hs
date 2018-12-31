{-# LANGUAGE UndecidableInstances #-}

module Chap6.Translate where

import Chap6.Frame (FrameOps, MonadFrame)
import Chap6.Temp (MonadTemp)
import GHC.Records
import qualified Chap6.Frame as Frame
import qualified Chap6.Temp as Temp

data Init level = Init
  { _parent  :: level
  , _name    :: Temp.Label
  , _formals :: [Bool]
  }

-- | `access` is not the same as `access` in Frame. It is usually a wrapper
-- around it that also includes information about the level.
class LevelOps access level | level -> access where
  outermost    :: level
  levelFormals :: level -> [access]

class (LevelOps access level, Monad m) =>
  MonadTranslate level access m | m -> access level where

  newLevel        :: Init level -> m level
  allocLocalLevel :: level -> Bool -> m access

data Level frame = Level
  { _parent  :: Level frame
  , _name    :: Temp.Label
  , _formals :: [Bool]
  , _frame   :: frame
  } | Outermost

instance (FrameOps access frame) =>
  LevelOps (Access frame access) (Level frame) where

  outermost = Outermost
  levelFormals Outermost = []
  levelFormals level     =
    fmap (Access level) . tail . Frame.formals . _frame $ level

data Access frame access = Access
  { _level  :: Level frame
  , _access :: access -- ^ Frame access
  }

newtype DeriveTranslate m a = DeriveTranslate (m a)
  deriving (Functor, Applicative, Monad)
instance (Monad m, MonadTemp m, MonadFrame access frame m) =>
  MonadTranslate (Level frame) (Access frame access)
    (DeriveTranslate m) where
  newLevel = DeriveTranslate . newLevel'
  allocLocalLevel l e = DeriveTranslate $ allocLocalLevel' l e

newLevel' :: (MonadTemp m, MonadFrame access frame m)
          => Init (Level frame) -> m (Level frame)
newLevel' init = do
  label <- Temp.newLabel
  frame <- Frame.newFrame (Frame.Init label (getField @"_formals" init))
  return Level
    { _parent  = getField @"_parent" init
    , _name    = getField @"_name" init
    , _formals = getField @"_formals" init
    , _frame   = frame
    }

allocLocalLevel' :: MonadFrame access frame m
                 => Level frame -> Bool -> m (Access frame access)
allocLocalLevel' level escapes =
  Access level <$> Frame.allocLocalFrame (_frame level) escapes
