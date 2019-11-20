module SpirV.Typed.Type

import SpirV.Raw.Options
import SpirV.Raw.Misc

%access public export

data TypeKind = KVoid | KScalar | KComposite

-- Type of variables
data VarType : TypeKind -> Type where
    TInt : (width : Nat) -> Signedness -> VarType KScalar
    TFloat : (width : Nat) -> VarType KScalar
    TStruct : List () -> VarType KComposite


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
