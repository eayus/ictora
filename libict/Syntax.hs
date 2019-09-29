module Syntax where

import Util

data Expr
    = Var Identifier
    | Lit Literal
    | App Expr Expr
    | Lam Identifier Expr       -- param, body
    | If Expr Expr Expr   -- cond, if body, else body
    deriving Show

data Literal
    = LInt Int
    | LBool Bool
    deriving Show

data FuncDef = FuncDef Identifier Expr -- function name, body
    deriving Show

type Program = [FuncDef]
