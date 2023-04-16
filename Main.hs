{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <&>" #-}

module Main where

import Control.Monad.Error
import Data.Functor qualified
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

apply :: String -> [Type0Val] -> ThrowsError Type0Val
apply fn args =
  maybe
    (throwError $ NotFunction "Unrecognized primitive function" fn)
    ($ args)
    $ lookup fn primitives

isString :: Type0Val -> Type0Val
isString (Str _) = Boolean True
isString _ = Boolean False

isNumber :: Type0Val -> Type0Val
isNumber (Number _) = Boolean True
isNumber (FloatN _) = Boolean True
isNumber _ = Boolean False

isSymbol :: Type0Val -> Type0Val
isSymbol (Atom _) = Boolean True
isSymbol _ = Boolean False

primitives :: [(String, [Type0Val] -> ThrowsError Type0Val)]
primitives =
  [ ("+", numericBinop (+)),
    ("-", numericBinop (-)),
    ("*", numericBinop (*)),
    ("/", numericBinop div),
    ("mod", numericBinop mod),
    ("quotient", numericBinop quot),
    ("remainder", numericBinop rem),
    ("sym?", unaryOp isSymbol),
    ("str?", unaryOp isString),
    ("num?", unaryOp isNumber)
  ]

unaryOp ::
  (Type0Val -> Type0Val) ->
  [Type0Val] ->
  ThrowsError Type0Val
unaryOp fn [param] = return $ fn param
unaryOp fn items = throwError $ NumArgs 1 items

numericBinop ::
  (Integer -> Integer -> Integer) ->
  [Type0Val] ->
  ThrowsError Type0Val
numericBinop op single@[_] = throwError $ NumArgs 2 single
numericBinop op params = mapM unpackNum params >>= (return . Number . foldl1 op)

unpackNum :: Type0Val -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (FloatN n) = return $ truncate n
unpackNum n = throwError $ TypeMismatch "number" n

eval :: Type0Val -> ThrowsError Type0Val
eval val@(Atom _) = return val
eval val@(Number _) = return val
eval val@(FloatN _) = return val
eval val@(Str _) = return val
eval val@(Charac _) = return val
eval val@(Boolean _) = return val
eval (List [Atom "quote", val]) = return val -- (quote exp)
eval (List (Atom fn : args)) =
  mapM eval args >>= apply fn -- (fn arg1 arg2 ...)
eval badForm =
  throwError $ BadSpecialForm "Unrecognized special form" badForm

readExpr :: String -> ThrowsError Type0Val
readExpr src = case parse parseExpr "type0" src of
  Left err -> throwError $ Parser err
  Right val -> return val

data Type0Error
  = NumArgs Integer [Type0Val]
  | TypeMismatch String Type0Val
  | Parser ParseError
  | BadSpecialForm String Type0Val
  | NotFunction String String
  | UnboundVar String String
  | Default String

showError :: Type0Error -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) =
  "Expected " ++ show expected ++ " args; found values " ++ stringifyList found
showError (TypeMismatch expected found) =
  "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

instance Show Type0Error where
  show = showError

instance Error Type0Error where
  noMsg = Default "An error has occurred"
  strMsg = Default

type ThrowsError = Either Type0Error

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

main :: IO ()
main = do
  args <- getArgs
  let res = fmap show $ readExpr (head args) >>= eval
  putStrLn $ extractValue $ trapError res