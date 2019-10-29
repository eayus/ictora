module SpirV.Type where

import SpirV.Options

data STypeKind = KVoid | KScalar | KComposite

type family IsVoid (t :: STypeKind) :: Bool where
    IsVoid KVoid = 'True
	IsVoid _     = 'False


data SType (t :: STypeKind) where
    TVoid :: SType NonScalar
	TInt :: Width -> Signedness -> SType Scalar
	TBool :: SType Scalar
	TFloat :: SType Scalar
	TVec :: IsVoid a ~ 'True => SType a -> SType NonScalar
