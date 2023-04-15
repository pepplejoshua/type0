module Main where

import Numeric (readDec, readHex, readOct)
import System.Environment (getArgs)
import Text.ParserCombinators.Parsec hiding (spaces)

data Type0Val
  = Atom String
  | List [Type0Val]
  | DotList [Type0Val] Type0Val
  | Number Integer
  | Str String
  | Charac Char
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
  res <- try (oneOf "\\\"") <|> oneOf "rnt"
  return $ case res of
    'r' -> '\r'
    'n' -> '\n'
    't' -> '\t'
    _ -> res

parseStringR5RS :: Parser Type0Val
parseStringR5RS = do
  char '"'
  x <- many $ escaped <|> noneOf "\""
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

parseNumberStd :: Parser Type0Val
parseNumberStd = do
  numTag <- try (string "#x") <|> try (string "#o") <|> return ""
  let base
        | numTag == "#x" = 16
        | numTag == "#o" = 8
        | otherwise = 10
  numStr <-
    if null numTag
      then many1 digit
      else many1 (oneOf "0123456789abcdefABCDEF")
  let num = case readDecOrHex base numStr of
        [(n, "")] -> n
        _ -> error "Invalid number"
  return $ Number num

readDecOrHex :: Int -> String -> [(Integer, String)]
readDecOrHex base str
  | base == 8 = readOct str
  | base == 10 = readDec str
  | base == 16 = readHex str
  | otherwise = error "Invalid base"

parseChar :: Parser Type0Val
parseChar = do
  _ <- char '#'
  _ <- char '\\'
  c <- anyChar
  c <- case c of
    't' -> return '\t'
    'n' -> return '\n'
    'r' -> return '\r'
    '"' -> return '\"'
    '\'' -> return '\''
    '\\' -> return '\\'
    'x' -> do
      hexDigits <- count 2 hexDigit
      case readHex hexDigits of
        [(n, "")] -> return $ toEnum n
        _ -> fail "Invalid hex escape"
    'u' -> do
      hexDigits <- count 4 hexDigit
      case readHex hexDigits of
        [(n, "")] -> return $ toEnum n
        _ -> fail "Invalid Unicode escape"
    _ -> return c
  return $ Charac c

parseExpr :: Parser Type0Val
parseExpr =
  parseNumberStd
    <|> parseChar
    <|> parseStringR5RS
    <|> parseAtom

readExpr :: String -> String
readExpr src = case parse (spaces >> parseExpr) "type0" src of
  Left err -> "No match: " ++ show err
  Right val -> case val of
    Atom s -> do
      "Atom " ++ s
    Number n -> do
      "Number " ++ show n
    Str ss -> do
      "Str " ++ ss
    Boolean True -> do
      "Boolean true"
    Boolean False -> do
      "Boolean false"
    Charac ch -> do
      "Character " ++ [ch]
    _ -> "Matched Input!"

main :: IO ()
main = do
  args <- getArgs
  putStrLn (readExpr (head args))