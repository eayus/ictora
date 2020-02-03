module Ictora.Ict.Lang

import public Data.Vect
import public Data.Fin
import public Ictora.Util
import public Ictora.Ict.Types
import public Ictora.Misc

%access public export
%default total

IScope : Type
IScope = Map String ITy


IGlobalScope : Type
IGlobalScope = (scope : IScope ** UniqKeys scope)


data IExpr : IScope -> ITy -> Type where
    ILit : Int -> IExpr scope ITInt
    IVar : Lookup vname scope ty -> IExpr scope ty
    IApp : IExpr scope (a ~> b) -> IExpr scope a -> IExpr scope b
    ILam : (vname : String)
        -> (a : ITy)
        -> IExpr ((vname, a) :: scope) b
        -> IExpr scope (a ~> b)
    ILet : (vname : String)
        -> IExpr scope a
        -> IExpr ((vname, a) :: scope) b
        -> IExpr scope b


data IProg : IGlobalScope -> Type where
    Nil : IProg (scope ** scopeUniq)
    IConsFunc : (funcName : String)
             -> (nk : NoKey funcName scope)
             -> IExpr scope t
             -> IProg ((funcName, t) :: scope ** UniqCons nk scopeUniq)
             -> IProg (scope ** scopeUniq)
