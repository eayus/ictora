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


data IExpr : IScope -> IGlobalScope -> ITy -> Type where
    ILit : Int -> IExpr locals globals ITInt
    ILocalVar : Lookup vname locals ty -> IExpr locals globals ty
    IGlobalVar : Lookup vname (fst globals) ty -> IExpr locals globals ty
    IApp : IExpr locals globals (a ~> b) -> IExpr locals globals a -> IExpr locals globals b
    ILam : (vname : String)
        -> (a : ITy)
        -> IExpr ((vname, a) :: locals) globals b
        -> IExpr locals globals (a ~> b)
    ILet : (vname : String)
        -> IExpr locals globals a
        -> IExpr ((vname, a) :: locals) globals b
        -> IExpr locals globals b


data IProg : IGlobalScope -> Type where
    Nil : IProg (scope ** scopeUniq)
    IConsFunc : (funcName : String)
             -> (nk : NoKey funcName scope)
             -> IExpr [] (scope ** scopeUniq) t
             -> IProg ((funcName, t) :: scope ** UniqCons nk scopeUniq)
             -> IProg (scope ** scopeUniq)
