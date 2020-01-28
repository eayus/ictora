module Ictora.Main.Parse

import public Ictora.Main.Lang

import Lightyear
import Lightyear.Core
import Lightyear.Char
import Lightyear.Strings

{--import public TParsec
import public TParsec.Running
import Data.NEList--}


%access public export
-- %default total


parseIdent : Parser String
parseIdent = do
    c <- letter
    cs <- many alphaNum
    pure . pack $ c :: cs


-- Ty  := Ty' -> Ty | Ty'
-- Ty' := ident
--     := (Ty)


parseTy : Parser Ty
parseTy = let parseNamed = NamedTy <$> parseIdent
              parseTy' = parens parseTy <|>| parseNamed
              parseArrow = const (~>) <$> token "->"
          in chainr1 parseTy' parseArrow



-- Expr  := Expr' Expr | Expr'
-- Expr' := \x : Ty => Expr
--       := (Expr)
--       := ident


parseExpr : Parser Expr
parseExpr = let parseVar = Var <$> parseIdent
                parseLam = do
                    token "\\"
                    name <- parseIdent
                    token ":"
                    ty <- parseTy
                    token "=>"
                    body <- parseExpr
                    pure $ Lam name ty body

                parseExpr' = parseVar <|>| parseLam <|>| parens parseExpr
                
                parseApp = const (App) <$> char ' '
            in chainl1 parseExpr' parseApp

