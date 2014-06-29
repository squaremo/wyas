module Error where

import Control.Monad.Error
import Text.ParserCombinators.Parsec

import Values

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
