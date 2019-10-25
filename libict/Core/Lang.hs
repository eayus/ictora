module Core.Lang where

import Core.Type

type VarIdentifier = String
type FuncIdentifier = String

data Literal = LInt Int

data Expr
    = Var VarIdentifier CType
    | Lit Literal
    -- | Func FuncIdentifier CType
    | App FuncIdentifier [Expr] CType
    | MkPair Expr Expr CType
    | DePair (VarIdentifier, VarIdentifier) (CType, CType) Expr Expr CType -- let (x, y) = pair in ...

-- Want to enforce the invariant that arity of function must equal number of expressions passed as args

data FuncDef = FuncDef FuncType FuncIdentifier [VarIdentifier] Expr

newtype Program = Program [FuncDef]



testCoreProg :: FuncDef
testCoreProg = FuncDef (FuncType TInt [TInt, TInt]) "double" ["x", "y"] (Lit $ LInt 5)
