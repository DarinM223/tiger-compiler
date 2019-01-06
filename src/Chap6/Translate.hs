module Chap6.Translate where

import Control.Monad.Reader
import Chap5.Symbol
import Chap6.Temp
import Chap6.Frame (FrameM (..))
import Chap6.Temp (TempM (..))
import GHC.Generics
import GHC.Records
import qualified Chap6.Frame as Frame
import qualified Chap6.Temp as Temp
import qualified Chap6.MipsFrame as Mips

data Init level = Init
  { _parent  :: level
  , _name    :: Temp.Label
  , _formals :: [Bool]
  }

-- | `access` is not the same as `access` in Frame. It is usually a wrapper
-- around it that also includes information about the level.
data TranslateM level access m = TranslateM
  { newLevel        :: Init level -> m level
  , allocLocalLevel :: level -> Bool -> m access
  , outermost       :: level
  , levelFormals    :: level -> [access]
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

translateMIO :: Monad m => TempM m -> FrameM access frame m
             -> TranslateM (Level frame) (Access frame access) m
translateMIO tempM frameM = TranslateM
  { newLevel        = newLevel' tempM frameM
  , allocLocalLevel = allocLocalLevel' frameM
  , outermost       = Outermost
  , levelFormals    = levelFormals'
  }
 where
  levelFormals' Outermost = []
  levelFormals' level =
    fmap (Access level) . tail . (formals frameM) . _frame $ level

newLevel' :: Monad m => TempM m -> FrameM access frame m
          -> Init (Level frame) -> m (Level frame)
newLevel' tempM frameM init = do
  label <- newLabel tempM
  frame <- (newFrame frameM) (Frame.Init label (getField @"_formals" init))
  return Level
    { _parent  = getField @"_parent" init
    , _name    = getField @"_name" init
    , _formals = getField @"_formals" init
    , _frame   = frame
    }

allocLocalLevel' :: Functor m => FrameM access frame m
                 -> Level frame -> Bool -> m (Access frame access)
allocLocalLevel' frameM level escapes =
  Access level <$> (allocLocalFrame frameM) (_frame level) escapes

-- Temporary example for how to use these effectful records, delete later
data Config = Config
  { _symRef   :: SymbolRef
  , _symTable :: SymbolTable
  , _tempRef  :: TempRef
  } deriving Generic

type Foo = ReaderT Config IO

foo :: IO ()
foo = config >>= \c -> flip runReaderT c $ do
  let tempM  = tempMIO
      frameM = Mips.frameMIO tempM
      transM = translateMIO tempM frameM
  _ <- newTemp tempM
  _ <- (newLevel transM) undefined
  undefined
 where
  config = Config <$> mkSymbolRef <*> mkSymbolTable <*> mkTempRef
