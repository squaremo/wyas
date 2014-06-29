module Eval where

import Control.Monad.Error

import Values
import Error

eval :: Val -> ThrowsError Val
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _)   = return val
eval (List [Atom "quote", val]) = return val
eval (List (h : t)) = do
  func <- eval h
  args <- mapM eval t
  apply func args
--eval (DottedList (func : args) rest) =
--  return $ apply (eval func) $ map eval $ args ++ (listify $ eval rest)
eval (Atom s) =
  maybe (throwError $ UnboundVar "Unbound variable" s) return $ lookup s primitives
eval bad = throwError $ BadSpecialForm "Malformed expression" bad

listify (List as) = as

apply :: Val -> [Val] -> ThrowsError Val
apply (Primitive _ f) args = return $ f args
apply f _ = throwError $ NotFunction "Attempt to apply non-function" $ show f

primitives :: [(String,  Val)]
primitives = [("+", numericBinOp "+" (+))]

numericBinOp name op =
  Primitive name (\as -> Number $ foldl1 op $ map unpackNum as)

unpackNum (Number i) = i
