module SpirV.Type where

import SpirV.Options
{--import Data.Kind

data TypeKind = KVoid | Scalar | Composite

data STypeKind :: TypeKind -> Type where
    SKVoid :: STypeKind KVoid
    SScalar :: STypeKind Scalar
    SComposite :: STypeKind Composite

data VarType :: TypeKind -> Type where
    TVoid :: SType KVoid
    TInt :: Int -> Signedness -> SType Scalar
    TBool :: SType Scalar
    TFloat :: SType Scalar
    TVec :: SType Scalar -> SType Composite
    TSum :: SType t1 -> SType t2 -> SType Composite
    TStruct :: [VarType] -> SType Composite--}


data VarType
    = TVoid
    | TInt Int Signedness
    | TBool
    | TFloat Int
    | TVec2 VarType
    | TVec3 VarType
    | TStruct [VarType]
    | TPtr VarType StorageClass
    deriving Eq

data FuncType = FuncType { ret :: VarType, params :: [VarType] } deriving Eq
