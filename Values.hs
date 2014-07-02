module Values where

import Text.Show.Functions
import Control.Monad.Error
import Text.ParserCombinators.Parsec
import Data.IORef

data Val = Atom String
         | List [Val]
         | DottedList [Val] Val
         | Number Integer
         | String String
         | Bool Bool
         | Primitive String ([Val] -> ThrowsError Val)
           -- as per the book, using a recod for the halibut
         | Func { params :: [String],
                  vararg :: Maybe String,
                  body :: [Val],
                  closure :: Env }
         | Undefined

-- The environment itself gets mutationed, and the individual cells
-- (variables) can get mutationed.
type Env = IORef [(String, IORef Val)]

unwordsList = unwords . map showVal

showVal (Atom a) = a
showVal (List es)  = "(" ++ unwordsList es ++ ")"
showVal (DottedList h t) =
  "(" ++ unwordsList h ++ " . " ++ showVal t ++ ")"
showVal (Number i) = show i
showVal (String s) = "\"" ++ s ++ "\""
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Primitive s fn) = "<primitive " ++ s ++ ">"
showVal (Func { params = args, vararg = varargs,
                body = body, closure = env }) =
  "(lambda (" ++ unwords args ++
  (case varargs of
    Nothing -> ""
    Just a  -> " . " ++ a) ++ ") ... )"
showVal Undefined = "<undefined>"

instance Show Val where show = showVal

-- Include errors here, since they are interdependent

data Err = WrongArity Integer [Val]
         | TypeMismatch String Val
         | ParseErr ParseError
         | BadSpecialForm String Val
         | NotFunction String String
         | UnboundVar String String
         | Default String
         deriving (Show)

-- This is to integrate with Haskell's special purpose error stuffs
instance Error Err where
  noMsg = Default "There arose an error"
  strMsg = Default

-- NB partially applied type constructor
type ThrowsError = Either Err

-- Convert (left) errors to (right) string values
trapError either = catchError either (return . show)

-- NB only defined for values, not errors; intended to be used after
-- trapError or similar
extractValue (Right val) = val
