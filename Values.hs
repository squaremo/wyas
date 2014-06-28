module Values where

data Val = Atom String
         | List [Val]
         | DottedList [Val] Val
         | Number Integer
         | String String
         | Bool Bool
         deriving (Show)
