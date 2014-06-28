module Values where

data Val = Atom String
         | List [Val]
         | DottedList [Val] Val
         | Number Integer
         | String String
         | Bool Bool
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

-- I've resisted instantiating Show with showVal, because I'd like to
-- see the internal structure too
