module Parser (doParse) where

import qualified Syntax

import Data.Void
import Text.Megaparsec

type Parser = Parsec Void String

testInput :: String
testInput = "f = g y z"

doParse :: IO ()
doParse = parseTest parseP testInput

parseP :: Parser Char
parseP = satisfy (\x -> True)
