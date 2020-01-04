module Ictora.GCore.Types

%access public export

data GVarTy : Type where
    GTInt : GVarTy
    GTBool : GVarTy
    GTFloat : GVarTy
    GTSum : List GVarTy -> GVarTy

record GFuncTy where
    constructor MkGFuncTy
    ret : GVarTy
    params : List GVarTy

data GTy : Type where
    GTyFunc : GFuncTy -> GTy
    GTyVar : GVarTy -> GTy

interpTy : GVarTy -> Type
interpTy GTInt = Int
interpTy GTBool = Bool
interpTy GTFloat = Double
interpTy (GTSum xs) = typeListTuple $ interpTyList xs
    where interpTyList : List GVarTy -> List Type
          interpTyList [] = []
          interpTyList (x :: xs) = interpTy x :: interpTyList xs

          typeListTuple : List Type -> Type
          typeListTuple [] = ()
          typeListTuple (x :: xs) = (x, typeListTuple xs)

