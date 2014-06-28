module Main where

import System.Environment
import Parse
import Values

main = do
  args <- getArgs
  putStrLn $ case readExpr (args !! 0) of
    Left err  -> "Error: " ++ show err
    Right val -> showVal val
