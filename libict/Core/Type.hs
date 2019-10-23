module Core.Type where

data CType
    = TFunc
    | TInt
    | TPair CType CType
    deriving (Eq, Show)

data FuncType = FuncType
    { returnType :: CType -- Return Type cannot be TFunc!!
    , paramTypes :: [CType] }
    deriving (Eq, Show)
