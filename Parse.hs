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

parseList = liftM List $ sepBy parseExpr spaces

parseDotted = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseExpr = parseNumber
            <|> parseAtom
            <|> parseString
            <|> parseQuoted
            <|> do char '('
                   x <- try parseList <|> parseDotted
                   char ')'
                   return x

parseExprs = endBy parseExpr spaces

runParser parser = parse parser "scheme"
