module Eval where

import Values

eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _)   = val
eval (List [Atom "quote", val]) = val
eval (List (func : args)) =
  apply (eval func) (map eval args)
--eval (DottedList (func : args) rest) =
--  apply (eval func) $ map eval args ++ eval rest
eval (Atom s) = maybe alwaysFalse id $ lookup s primitives

alwaysFalse = Primitive "alwaysFalse" (\ _ -> Bool False)

apply :: Val -> [Val] -> Val
apply (Primitive _ f) args = f args

primitives :: [(String,  Val)]
primitives = [("+", numericBinOp "+" (+))]

numericBinOp name op =
  Primitive name (\as -> Number $ foldl1 op $ map unpackNum as)

unpackNum (Number i) = i
