module Core.Type where

data VarType
    = TFunc
    | TInt
    | TPair VarType VarType
    deriving (Eq, Show)

data FuncType = FuncType
    { returnType :: CType -- Return Type cannot be TFunc!!
    , paramTypes :: [CType] }
    deriving (Eq, Show)
