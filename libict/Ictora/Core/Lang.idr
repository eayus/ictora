module Ictora.Core.Lang

import public Data.Fin
import public Data.Vect
import public Ictora.Core.Types
import public Ictora.Util

%access public export


CFuncId : Type
CFuncId = String


data CExpr : (locals : Vect n CTy) -> (globals : Vect m (CFuncId, CTy)) -> CTy -> Type where
    CLocalVar : IndexIs i local ty -> CExpr local global ty
    CGlobalVar : LookupIs vname globals ty -> CExpr local global ty
    CLit : Int -> CExpr local global CTInt
    CApp : CExpr local global (a ~> b) -> CExpr local global a -> CExpr local global b
    CLam : CExpr (a :: local) global b -> CExpr local global (a ~> b)
    CLet : CExpr local global a -> CExpr (a :: local) global b -> CExpr local global b


data CProgram : (globals : Vect m (CFuncId, CTy)) -> Type where
    IEmptyProg : CProgram globals
    IConsFunc : (fname : CFuncId)
             -> CExpr [] globals ty
             -> CProgram ((fname, ty) :: globals)
             -> CProgram globals
