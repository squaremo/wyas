module Eval where

import Control.Monad.Error

import Values

eval :: Val -> ThrowsError Val
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _)   = return val
-- quotation
eval (List [Atom "quote", val]) = return val
-- reference
eval (Atom s) =
  maybe (throwError $ UnboundVar "Unbound variable" s) return $ lookup s primitives
-- if, in two forms
eval (List [Atom "if", pred, conseq]) = do
  res <- eval pred
  case res of
    Bool False -> return Undefined
    _else      -> eval conseq
eval (List [Atom "if", pred, conseq, alter]) = do
  res <- eval pred
  case res of
    Bool False -> eval alter
    _else      -> eval conseq
-- begin
eval (List ((Atom "begin") : rest)) = evalProgn rest
-- any list forms left are applications
eval (List (h : t)) = do
  func <- eval h
  args <- mapM eval t
  apply func args
--eval (DottedList (func : args) rest) =
--  return $ apply (eval func) $ map eval $ args ++ (listify $ eval rest)

eval bad = throwError $ BadSpecialForm "Malformed expression" bad

evalProgn [] = return Undefined
evalProgn [last] = eval last
evalProgn (first : rest) = eval first >> evalProgn rest

apply :: Val -> [Val] -> ThrowsError Val
apply (Primitive _ f) args = f args
apply f _ = throwError $ NotFunction "Attempt to apply non-function" $ show f

primitives :: [(String,  Val)]
primitives = [("+", numericBinOp "+" (+)),
              ("-", numericBinOp "-" (-)),
              ("<", boolBinOp unpackNum "<" (<))]

numericBinOp name op =
  let fn = \vs -> do
        nums <- mapM unpackNum vs
        let v = foldl1 op nums
        return $ Number v in
  Primitive name fn

-- Slight departure again: I let comparisons run to >2 arguments too
-- (Schemes tend to do this)
boolBinOp unpack name op =
  let fn = \vs -> do
        args <- mapM unpack vs
        let v = and $ zipWith op args (drop 1 args)
        return $ Bool v in
  Primitive name fn

unpackNum (Number i) = return i
unpackNum bad = throwError $ TypeMismatch "Expected a number" bad
