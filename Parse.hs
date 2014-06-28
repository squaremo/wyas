module Parse where

import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import Values

symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

-- spaces is hidden because, apparently, it doesn't do what we
-- want. Here's a replacement.

spaces = skipMany1 space

parseString = do
  char '"'
  x <- many (noneOf "\"")
  char '"'
  return $ String x

parseAtom = do
  first <- letter <|> symbol
  rest <- many $ letter <|> symbol <|> digit
  let a = first : rest
  return $ case a of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom a


-- I need my number reader to operate on the Parser monad, hence the
-- liftM
parseNumber =
  liftM (Number . read) $ many1 digit

parseExpr = parseNumber <|> parseAtom <|> parseString

-- NB the monadic bind-without-value; spaces and symbol are both
-- instances of the Parse monad, and we want to throw out the result
-- of spaces
program = (spaces >> parseExpr)

readExpr :: String -> String
readExpr input = case parse program "lisp" input of
  Left err  -> "No match: " ++ show err
  Right val -> "Found value"
