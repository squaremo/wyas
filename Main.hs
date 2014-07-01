module Main where

import System.Environment
import System.IO
import Control.Monad.Error

import Parse
import Values
import Eval

flushStr s = putStr s >> hFlush stdout

readPrompt p = flushStr p >> getLine

readExpr s = case parseProgram s of
  Left err  -> throwError $ ParseErr err
  Right val -> return val

evalString :: String -> IO String
evalString s = return $ extractValue $
               trapError (liftM showVal $ readExpr s >>= eval)

evalAndPrint :: String -> IO ()
evalAndPrint s = evalString s >>= putStrLn

until_ exitp prompt action = do
  result <- prompt
  if exitp result
    then return ()
    else action result >> until_ exitp prompt action

runRepl =
  until_ (== "quit") (readPrompt "repl> ") evalAndPrint

main = do
  args <- getArgs
  case length args of
    0 -> runRepl
    1 -> evalAndPrint (args !! 0)
    otherwise -> putStrLn "Expects either an expression or nothing"
