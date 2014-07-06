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

runFile args = do
  env <- initEnv >>= flip bindVars [("argv", List $ map String $ drop 1 args)]
  _ <- runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)])
  return ()

runRepl =
  initEnv >>= until_ (== "quit") (readPrompt "repl> ") . evalAndPrint

main = do
  args <- getArgs
  if null args then runRepl else runFile $ args
