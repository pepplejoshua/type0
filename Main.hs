module Main where

import Numeric (readDec, readFloat, readHex, readOct)
import System.Environment (getArgs)
import Text.ParserCombinators.Parsec hiding (spaces)

data Type0Val
  = Atom String
  | List [Type0Val]
  | DotList [Type0Val] Type0Val
  | Number Integer
  | FloatN Double
  | Str String
  | Charac Char
  | Boolean Bool

instance Show Type0Val where show = stringify

stringify :: Type0Val -> String
stringify (Atom s) = s
stringify (Number n) = show n
stringify (FloatN n) = show n
stringify (Str ss) = "\"" ++ ss ++ "\""
stringify (Boolean True) = "#t"
stringify (Boolean False) = "#f"
stringify (List items) = "(" ++ stringifyList items ++ ")"
stringify (DotList head tail) = "(" ++ stringifyList head ++ " . " ++ stringify tail ++ ")"
stringify (Charac ch) = "'" ++ [ch] ++ "'"

stringifyList :: [Type0Val] -> String
stringifyList = unwords . map stringify

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

  decimalPoint <- try (char '.') <|> return '\0'
  rest <-
    if decimalPoint == '.'
      then many1 digit
      else return ""

  let final
        | null rest = numStr
        | otherwise = numStr ++ [decimalPoint] ++ rest

  if null rest
    then case readDecOctHex base final of
      [(n, "")] -> return $ Number n
      _ -> error "Invalid number"
    else case readFloatNum final of
      [(n, "")] -> return $ FloatN n
      _ -> error "Invalid float"

readFloatNum :: String -> [(Double, String)]
readFloatNum str = case readDec str of
  [(n, "")] -> [(fromIntegral n, "")]
  _ -> case readFloat str of
    [(n, "")] -> [(n, "")]
    _ -> [(0, str)]

readDecOctHex :: Int -> String -> [(Integer, String)]
readDecOctHex base str
  | base == 8 = readOct str
  | base == 10 = readDec str
  | base == 16 = readHex str
  | otherwise = [(0, str)]

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

parseList :: Parser Type0Val
parseList = List <$> sepBy parseExpr spaces

parseDotList :: Parser Type0Val
parseDotList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DotList head tail

parseQuoted :: Parser Type0Val
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseUnquote :: Parser Type0Val
parseUnquote = do
  char ','
  x <- parseExpr
  return $ List [Atom "unquote", x]

parseExpr :: Parser Type0Val
parseExpr =
  parseNumberStd
    <|> parseChar
    <|> parseStringR5RS
    <|> parseAtom
    <|> parseQuoted
    <|> parseUnquote
    <|> do
      char '('
      x <- try parseList <|> parseDotList
      char ')'
      return x

readExpr :: String -> String
readExpr src = case parse parseExpr "type0" src of
  Left err -> "No match: " ++ show err
  Right val -> show val

main :: IO ()
main = do
  args <- getArgs
  putStrLn (readExpr (head args))