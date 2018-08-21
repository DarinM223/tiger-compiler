module Chap2.Driver where

import Chap2.Lexer

printTokens :: [Token] -> IO ()
printTokens = mapM_ (putStrLn . show)

runApp :: IO ()
runApp = do
  s <- getContents
  printTokens $ alexScanTokens s
