module Ictora.Core.Types

%access public export

infixr 2 ~>

data CTy : Type where
    CTInt : CTy
    CTFloat : CTy
    (~>) : CTy -> CTy -> CTy


data LitType : CTy -> Type where
    IntIsLit : LitType CTInt
    FloatIsLit : LitType CTFloat


interpCTy : LitType t -> Type
interpCTy IntIsLit = Int
interpCTy FloatIsLit = Double
