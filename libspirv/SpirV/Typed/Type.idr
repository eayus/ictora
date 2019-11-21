module SpirV.Typed.Type

import SpirV.Raw.Options
import SpirV.Raw.Misc

--TODO: Need to extend our typemap to account for the AccessType of variables too. (AccessType = { Function, Input, Output, ...})

%access public export

data TypeKind = KVoid | KScalar | KComposite | KPtr

-- Type of variables
data VarType : TypeKind -> Type where
    TInt : (width : Nat) -> Signedness -> VarType KScalar
    TFloat : (width : Nat) -> VarType KScalar
    TStruct : List (t : TypeKind ** VarType t) -> VarType KComposite
    TPtr : VarType t -> VarType KPtr


-- Type of functions
record FuncType where
    constructor MkFuncType
    retType : VarType a
    paramTypes : List (t : TypeKind ** VarType t)


IdrisVarType : VarType KScalar -> Type
IdrisVarType (TInt _ Signed) = Int
IdrisVarType (TInt _ Unsigned) = Nat
IdrisVarType (TFloat _) = Double


scalarToLit : (t : VarType KScalar) -> IdrisVarType t -> Literal
scalarToLit (TInt _ Signed) = IntLit
scalarToLit (TInt _ Unsigned) = IntLit . cast
scalarToLit (TFloat _) = FloatLit


Eq TypeKind where
    KVoid == KVoid = True
    KScalar == KScalar = True
    KComposite == KComposite = True
    _ == _ = False


varTypeEq : VarType a -> VarType b -> Bool
varTypeEq (TInt w1 s1) (TInt w2 s2) = (w1 == w2) && (s1 == s2)
varTypeEq (TFloat w1) (TFloat w2) = (w1 == w2)
varTypeEq (TStruct _) (TStruct _) = True
varTypeEq _ _  = False

vtDpEq : (t : TypeKind ** VarType t) -> (s : TypeKind ** VarType s) -> Bool
vtDpEq (_ ** vt1) (_ ** vt2) = varTypeEq vt1 vt2

funcTypeEq : FuncType -> FuncType -> Bool
funcTypeEq (MkFuncType ret1 params1) (MkFuncType ret2 params2)
    = varTypeEq ret1 ret2 && all id (zipWith vtDpEq params1 params2)

Eq FuncType where
    (==) = funcTypeEq
