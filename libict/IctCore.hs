module IctCore where

type Identifier = String

data TType = TInt | TFunc TType TType
    deriving Show

data Program a = [FuncDef a]

data FuncDef a = FuncDef [Identifier] (Expr a)

data Expr a
    = Var Identifier a
    | Lit Int a
    | Add Expr Expr a
    | App Expr Expr a
    deriving Show


