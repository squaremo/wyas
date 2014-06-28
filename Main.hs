module Main where

import System.Environment
import Parse
import Values
import Eval

evalString s = case readExpr s of
    Left err  -> "Error: " ++ show err
    Right val -> showVal $ eval val

main = getArgs >>= putStrLn . evalString . head
