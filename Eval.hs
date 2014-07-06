module Eval where

import Control.Monad.Error
import Data.IORef
import System.IO

import Values
import Parse

-- I effectively have two error monads; this lifts values of the
-- "inner" to those of the "outer", so they can be dealt with
-- uniformly
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

readOrThrow parser s = case runParser parser s of
  Left err -> throwError $ ParseErr err
  Right val -> return val

readExpr :: String -> ThrowsError Val
readExpr = readOrThrow parseExpr

readExprs :: String -> ThrowsError [Val]
readExprs = readOrThrow parseExprs

-- Environments

nullEnv :: IO Env
nullEnv = newIORef []

initEnv :: IO Env
initEnv = nullEnv >>=
          (flip bindVars $
           (map (mkFunc Primitive) primitives) ++
           (map (mkFunc IOPrimitive) ioprimitives))
  where mkFunc constructor (var, func) = (var, constructor var func)

isBound envRef var = readIORef envRef >>=
                  return . maybe False (const True) . lookup var

getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Unknown variable" var)
    (liftIO . readIORef)
    (lookup var env)

setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Unknown variable" var)
    (liftIO . (flip writeIORef value))
    (lookup var env)
  return value

defineVar envRef var value = do
  exists <- liftIO $ isBound envRef var
  if exists
     -- must return the value to put in the right monad?
    then setVar envRef var value >> return value
    else liftIO $ do
      valueRef <- newIORef value
      env <- readIORef envRef
      writeIORef envRef ((var, valueRef) : env)
      return value

-- For using in applications. Note that a new ref is created.
bindVars envRef bindings =
  readIORef envRef >>= extendEnv bindings >>= newIORef
  where extendEnv bindings env = liftM (++ env) (mapM newBinding bindings)
        newBinding (var, value) = do ref <- newIORef value
                                     return (var, ref)

eval :: Env -> Val -> IOThrowsError Val
eval _ val@(String _) = return val
eval _ val@(Number _) = return val
eval _ val@(Bool _)   = return val
-- quotation
eval _ (List [Atom "quote", val]) = return val
-- reference
eval env (Atom s) = getVar env s
-- if, in two forms
eval env (List [Atom "if", pred, conseq]) = do
  res <- eval env pred
  case res of
    Bool False -> return Undefined
    _else      -> eval env conseq
eval env (List [Atom "if", pred, conseq, alter]) = do
  res <- eval env pred
  case res of
    Bool False -> eval env alter
    _else      -> eval env conseq
-- begin
eval env (List ((Atom "begin") : rest)) = evalProgn env rest
eval env (List [Atom "set!", Atom var, form]) =
  eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
  eval env form >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
  mkLambda env params body
eval env (List (Atom "lambda" : DottedList params (Atom varargs) : body)) =
  mkVarargsLambda varargs env params body
eval env (List (Atom "lambda" : Atom varargs : body)) =
  mkVarargsLambda varargs env [] body

-- any list forms left are applications
eval env (List (h : t)) = do
  func <- eval env h
  args <- mapM (eval env) t
  apply func $ args
--eval (DottedList (func : args) rest) =
--  return $ apply (eval func) $ map eval $ args ++ (listify $ eval rest)

eval env bad = throwError $ BadSpecialForm "Malformed expression" bad

evalProgn env [] = return Undefined
evalProgn env exprs =
  liftM last $ mapM (eval env) exprs

apply :: Val -> [Val] -> IOThrowsError Val
apply (Primitive _ f) args = liftThrows $ f args
apply (IOPrimitive _ f) args = f args
apply (Func params varargs body closure) args =
  if num params /= num args && varargs == Nothing
  then throwError $ WrongArity (num params) args
  else
    do env0 <- (liftIO $ bindVars closure $ zip params args)
       env1 <- bindVarArgs varargs env0
       evalProgn env1 body
  where remainingArgs = drop (length params) args
        num = toInteger . length
        bindVarArgs arg env = case arg of
          Nothing -> return env
          Just v -> liftIO $ bindVars env [(v, List $ remainingArgs)]
apply f _ = throwError $ NotFunction "Attempt to apply non-function" $ showVal f

mkFunc varargs env params body =
  return $ Func (map atomName params) varargs body env
mkLambda = mkFunc Nothing
mkVarargsLambda = mkFunc . Just

atomName (Atom s) = s

primitives = [("+", numericBinOp (+)),
              ("-", numericBinOp (-)),
              ("<", boolBinOp unpackNum (<)),
              ("cons", cons),
              ("car", car),
              ("cdr", cdr),
              ("eqv?", eqv)]

ioprimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closeProc),
                ("close-output-port", closeProc),
                ("read", readProc),
                ("write", writeProc)]

-- This isn't quite R5RS apply
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args

makePort mode [String filename] =
  liftM Port $ liftIO $ openFile filename mode

readProc [] = readProc [Port stdin]
readProc [Port handle] =
  (liftIO $ hGetLine handle) >>= liftThrows . readExpr

writeProc [val] = writeProc [val, Port stdout]
writeProc [val, Port handle] =
  liftIO $ hPrint handle val >> (return $ Bool True)

closeProc [Port handle] =
  liftIO $ hClose handle >> (return $ Bool True)

numericBinOp op vs =
  do
    nums <- mapM unpackNum vs
    let v = foldl1 op nums
    return $ Number v

-- Slight departure again: I let comparisons run to >2 arguments too
-- (Schemes tend to do this)
boolBinOp unpack op vs =
  do
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
