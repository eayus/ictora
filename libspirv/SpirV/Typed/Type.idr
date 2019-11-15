module SpirV.Typed.Type

import SpirV.Raw.Options

data TypeKind = KVoid | KScalar | KComposite
    constructor MkVarTypeProps

-- Type of variables
data VarType : TypeKind -> Type where
    TInt : (width : Nat) -> Signedness -> VarType KScalar
    TStruct : List () -> VarType KComposite

