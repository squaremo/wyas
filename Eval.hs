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
              ("<", boolBinOp unpackNum "<" (<)),
              ("cons", Primitive "cons" cons),
              ("car", Primitive "car" car),
              ("cdr", Primitive "cdr" cdr),
              ("eqv?", Primitive "eqv?" eqv)]

numericBinOp name op =
  Primitive name fn
  where fn = \vs -> do
          nums <- mapM unpackNum vs
          let v = foldl1 op nums
          return $ Number v

-- Slight departure again: I let comparisons run to >2 arguments too
-- (Schemes tend to do this)
boolBinOp unpack name op =
  Primitive name fn
  where fn = \vs -> do
          args <- mapM unpack vs
          let v = and $ zipWith op args (drop 1 args)
          return $ Bool v

car [(List (h : _))] = return h
car [(DottedList (h : _) _)] = return h
car [other] = throwError $ TypeMismatch "expected pair" other
car bad = throwError $ WrongArity 1 bad

-- I don't know why the book has a separate case for nil (List []),
-- surely this covers it?
cons [x, List xs] = return $ List (x : xs)
cons [x, DottedList xs y] = return $ DottedList (x : xs) y
cons [x, y] = return $ DottedList [x] y
cons bad = throwError $ WrongArity 2 bad

cdr [List (_ : t)] = return $ List t
cdr [DottedList [_] y] = return y
cdr [DottedList (_ : t) y] = return $ DottedList t y
cdr [other] = throwError $ TypeMismatch "expected pair" other
cdr bad = throwError $ WrongArity 1 bad

eqv [Atom a, Atom b]     = return $ Bool $ a == b
eqv [Number a, Number b] = return $ Bool $ a == b
eqv [String a, String b] = return $ Bool $ a == b
eqv [Bool a, Bool b]     = return $ Bool $ a == b
eqv [List a, List b]     =
  return $ Bool $ (length a == length b) && (and $ zipWith eqv_ a b)
  where eqv_ a b = case eqv [a, b] of
          Left err -> False
          Right (Bool val) -> val
eqv [DottedList as a, DottedList bs b] =
  -- this is pretty awful, I wonder if there's a better way
  eqv [List (as ++ [a]), List (bs ++ [b])]
eqv [_, _] = return $ Bool False
eqv bad = throwError $ WrongArity 2 bad

unpackNum (Number i) = return i
unpackNum bad = throwError $ TypeMismatch "Expected a number" bad
