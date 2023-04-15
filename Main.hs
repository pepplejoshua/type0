module Main where

import System.Environment (getArgs)
import Text.ParserCombinators.Parsec hiding (spaces)

data Type0Val
  = Atom String
  | List [Type0Val]
  | DotList [Type0Val] Type0Val
  | Number Integer
  | Str String
  | Boolean Bool

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~#"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser Type0Val
parseString = do
  char '"'
  x <- many (noneOf "\"")
  char '"'
  return $ Str x

escaped :: Parser Char
escaped = do
  char '\\'
  char '"'
  return '"'

escapedSpaces :: Parser Char
escapedSpaces = do
  oneOf "\r\n\t\\"

parseStringR5RS :: Parser Type0Val
parseStringR5RS = do
  char '"'
  x <- many $ escapedSpaces <|> escaped <|> noneOf "\""
  char '"'
  return $ Str x

parseAtom :: Parser Type0Val
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ case atom of
    "#t" -> Boolean True
    "$f" -> Boolean False
    _ -> Atom atom

parseNumber :: Parser Type0Val
parseNumber = Number . read <$> many1 digit

parseNumber2 :: Parser Type0Val
parseNumber2 = do
  digits <- many1 digit
  let num = read digits :: Integer
  return $ Number num

parseNumber3 :: Parser Type0Val
parseNumber3 = many1 digit >>= \digits -> return $ Number (read digits :: Integer)

parseExpr :: Parser Type0Val
parseExpr =
  parseAtom
    <|> parseStringR5RS
    <|> parseNumber3

readExpr :: String -> String
readExpr src = case parse (spaces >> parseExpr) "type0" src of
  Left err -> "No match: " ++ show err
  Right val -> "Found value"

main :: IO ()
main = do
  args <- getArgs
  putStrLn (readExpr (head args))