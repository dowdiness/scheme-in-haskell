module Lib where

import           Control.Monad
import           GHC.IO.Device                 (IODevice (dup))
import           Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Number Integer
    | String String
    | Bool Bool

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

showVal :: LispVal -> String
showVal (Atom name) = name
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ "." ++ showVal tail ++ ")"
showVal (Number num) = show num
showVal (String str) = "\"" ++ str ++ "\""
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"

instance Show LispVal where show = showVal

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many (noneOf "\"")
    char '"'
    return $ String x

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuated :: Parser LispVal
parseQuated = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom
    <|> parseString
    <|> parseNumber
    <|> parseQuated
    <|> do
        char '('
        x <- try parseList <|> parseDottedList
        char ')'
        return x

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err  -> "No match " ++ show err
    Right val -> "Found " ++ show val
