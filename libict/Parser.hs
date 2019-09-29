module Parser (doParse) where

import Syntax
import Util

import Data.Void
import Data.Functor (($>))
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

testInput :: String
testInput = "long_name = True\n"

doParse :: IO ()
doParse = parseTest parseFuncDef testInput


parseIdentifier :: Parser Identifier
parseIdentifier = do
    x <- letterChar
    xs <- many (letterChar <|> digitChar <|> char '_')
    return $ x : xs

parseInt :: Parser Literal
parseInt = LInt <$> L.decimal

parseBool :: Parser Literal
parseBool = LBool <$> (parseTrue <|> parseFalse)
    where parseTrue = string "True" $> True
          parseFalse = string "False" $> False

parseLit :: Parser Expr
parseLit = Lit <$> (parseInt <|> parseBool)

parseLam :: Parser Expr
parseLam = do
    char '\\'
    param <- parseIdentifier
    string " => "
    body <- parseExpr
    return $ Lam param body

parseTerm :: Parser Expr
parseTerm = (between (char '(') (char ')') parseExpr) <|> parseLam <|> parseLit <|> (Var <$> parseIdentifier)

parseExpr :: Parser Expr
parseExpr = do
    terms <- parseTerm `sepBy1` (char ' ')
    return $ foldl1 App terms

parseFuncDef :: Parser FuncDef
parseFuncDef = do
    name <- parseIdentifier
    string " = "
    body <- parseExpr
    eol
    return $ FuncDef name body

parseProgram :: Parser Program
parseProgram = many parseFuncDef
