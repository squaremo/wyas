module Main where

import System.Environment
import Control.Monad.Error
import System.IO

import Parse
import Values
import Eval

-- for the outermost IO action
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

flushStr s = putStr s >> hFlush stdout

readPrompt p = flushStr p >> getLine

evalString :: Env -> String -> IO String
evalString env s =
  runIOThrows $ liftM showVal $ (liftThrows $ readExpr s) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env s = evalString env s >>= putStrLn

until_ exitp prompt action = do
  result <- prompt
  if exitp result
    then return ()
    else action result >> until_ exitp prompt action

runExpr expr = nullEnv >>= flip evalAndPrint expr

runRepl =
  initEnv >>= until_ (== "quit") (readPrompt "repl> ") . evalAndPrint

main = do
  args <- getArgs
  case length args of
    0 -> runRepl
    1 -> runExpr (args !! 0)
    otherwise -> putStrLn "Expects either an expression or nothing"
