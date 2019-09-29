module Syntax where

import Util

data Expr
    = Var Name
    | Lit Literal
    | App Expr Expr
    | Abs Name Expr       -- param, body
    | If Expr Expr Expr   -- cond, if body, else body

data Literal
    = LInt Int
    | LBool Bool

data FuncDef = FuncDef Name Expr -- function name, body

type Program = [FuncDef]
