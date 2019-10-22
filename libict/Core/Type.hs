module Core.Type where

data CType
    = TFunc
    | TInt
    | TPair CType CType
    deriving Eq

data FuncType = FuncType CType [CType] -- Return Type cannot be TFunc!!
