module Main where

import System.Environment
import Control.Monad.Error

import Parse
import Values
import Eval

evalString :: String -> ThrowsError Val
evalString s = case readExpr s of
    Left err  -> throwError $ ParseErr err
    Right val -> eval val

main = do
  args <- getArgs
  evaled <- return $ liftM showVal $ evalString (args !! 0)
  putStrLn $ extractValue $ trapError evaled
