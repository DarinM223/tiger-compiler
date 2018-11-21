module Tests (allTests) where

import Test.Tasty
import qualified ParserTest as P
import qualified TypeCheckTest as T

allTests :: TestTree
allTests = testGroup "All tests"
  [ P.tests
  , T.tests
  ]
