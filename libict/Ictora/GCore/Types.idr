module Ictora.GCore.Types

import Data.Vect
import Data.HVect

%access public export

data GVarTy : Type where
    GTInt : GVarTy
    GTBool : GVarTy
    GTFloat : GVarTy
    GTSum : (numFields : Nat) -> Vect numFields GVarTy -> GVarTy

record GFuncTy where
    constructor MkGFuncTy
    ret : GVarTy
    arity : Nat
    params : Vect arity GVarTy

data GTy : Type where
    GTyFunc : GFuncTy -> GTy
    GTyVar : GVarTy -> GTy

InterpTy : GVarTy -> Type
InterpTy GTInt = Int
InterpTy GTBool = Bool
InterpTy GTFloat = Double
InterpTy (GTSum len xs) = assert_total $ HVect (map InterpTy xs)
