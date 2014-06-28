module Values where

import Text.Show.Functions

data Val = Atom String
         | List [Val]
         | DottedList [Val] Val
         | Number Integer
         | String String
         | Bool Bool
         | Primitive String ([Val] -> Val)
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

-- I've resisted instantiating Show with showVal, because I'd like to
-- see the internal structure too
