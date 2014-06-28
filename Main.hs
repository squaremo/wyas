module Main where

import System.Environment
import Parse

main = do
  args <- getArgs
  putStrLn $ readExpr (args !! 0)
