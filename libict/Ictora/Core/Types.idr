module Ictora.Core.Types

%access public export

infixr 2 ~>

data CType : Type where
    CTInt : CType
    CTFloat : CType
    (~>) : CType -> CType -> CType
