module Ictora.GCore.Types

import Data.Vect
import Data.HVect
import Data.Fin

%access public export

data VecSize = Two | Three | Four


data GVarTy : Type where
    GTInt : GVarTy
    GTBool : GVarTy
    GTFloat : GVarTy
    GTSum : (numFields : Nat) -> Vect numFields GVarTy -> GVarTy
    GTVec : VecSize -> GVarTy


record GFuncTy where
    constructor MkGFuncTy
    ret : GVarTy
    arity : Nat
    params : Vect arity GVarTy


data GTy : Type where
    GTyFunc : GFuncTy -> GTy
    GTyVar : GVarTy -> GTy


sizeToNat : VecSize -> Nat
sizeToNat Two = 2
sizeToNat Three = 3
sizeToNat Four = 4


data GIsLiteral : GVarTy -> Type where
    GTIntIsLit : GIsLiteral GTInt
    GTBoolIsLit : GIsLiteral GTBool
    GTFloatIsLit : GIsLiteral GTFloat


InterpTy : (t : GVarTy) -> {auto p : GIsLiteral t} -> Type
InterpTy GTInt = Int
InterpTy GTBool = Bool
InterpTy GTFloat = Double
{--InterpTy (GTSum len xs) = assert_total $ HVect (map InterpTy xs)
InterpTy (GTVec size) = Vect (sizeToNat size) Double--}
