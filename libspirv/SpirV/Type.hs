module SpirV.Type where

import SpirV.Options

data TypeKind = KVoid | Scalar | Composite

data STypeKind :: TypeKind -> Type where
    SKVoid :: STypeKind KVoid
    SScalar :: STypeKind Scalar
    SComposite :: STypeKind Composite

data SType :: TypeKind -> Type where
    TVoid :: SType KVoid
    TInt :: Int -> Signedness -> SType Scalar
    TBool :: SType Scalar
    TFloat :: SType Scalar
    TVec :: SType Scalar -> SType Composite
    TSum :: SType t1 -> SType t2 -> SType Composite
