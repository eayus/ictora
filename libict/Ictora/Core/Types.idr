module Ictora.Core.Types

%access public export

data CType : Type where
    CTInt : CType
    CTFloat : CType
    CTArr : CType -> CType -> CType
