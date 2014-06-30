module Values where

import Text.Show.Functions
import Control.Monad.Error
import Text.ParserCombinators.Parsec

data Val = Atom String
         | List [Val]
         | DottedList [Val] Val
         | Number Integer
         | String String
         | Bool Bool
         | Primitive String ([Val] -> ThrowsError Val)
         | Undefined
         deriving (Show)

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
showVal Undefined = "<undefined>"

-- I've resisted instantiating Show with showVal, because I'd like to
-- see the internal structure too

-- Include errors here, since they are interdependent

data Err = Arity Integer [Val]
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

trapError either = catchError either (return . show)

-- NB only defined for values, not errors
extractValue (Right val) = val
