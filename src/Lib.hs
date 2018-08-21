module Lib
  ( runApp
  )
where

-- import qualified Chap1 as Chap1
import qualified Chap2.Driver as Chap2

runApp :: IO ()
runApp = Chap2.runApp
