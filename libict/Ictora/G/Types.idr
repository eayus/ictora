module Ictora.G.Types

import public Data.Vect

%access public export


data GTy : Type where
    GTInt : GTy
    GTFloat : GTy


record GFuncTy where
    constructor MkGFuncTy
    params : Vect arity GTy
    res : GTy
