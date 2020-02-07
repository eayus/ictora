module Ictora.Core.Lang

import public Data.Fin
import public Data.Vect
import public Ictora.Core.Types
import public Ictora.Util

%access public export


data CExpr : (locals : Vect n CTy) -> (globals : Vect m CTy) -> CTy -> Type where
    CLocalVar : IndexIs i local ty -> CExpr local global ty
    CGlobalVar : IndexIs i global ty -> CExpr local global ty
    CLit : Int -> CExpr local global CTInt
    CApp : CExpr local global (a ~> b) -> CExpr local global a -> CExpr local global b
    CLam : CExpr (a :: local) global b -> CExpr local global (a ~> b)
    CLet : CExpr local global a -> CExpr (a :: local) global b -> CExpr local global b


data CProg : (globals : Vect m CTy) -> Type where
    CEmptyProg : CProg globals
    CConsFunc : CExpr [] globals ty
             -> CProg (ty :: globals)
             -> CProg globals




data NoLam : CExpr locals global ty -> Type where
    NLLocalVar : NoLam (CLocalVar _)
    NLGlobalVar : NoLam (CGlobalVar _)
    NLLit : NoLam (CLit _)
    NLApp : NoLam l -> NoLam r -> NoLam (CApp l r)
    NLLet : NoLam e1 -> NoLam e2 -> NoLam (CLet e1 e2)

data NoInnerLam : CExpr locals globals ty -> Type where
    NoneAtAll : NoLam e -> NoInnerLam e
    OuterLam : NoInnerLam body -> NoInnerLam (CLam body)


data LamLifted : CProg globals -> Type where
    LLEmpty : LamLifted CEmptyProg
    LLConsFunc : NoInnerLam f -> LamLifted p -> LamLifted (CConsFunc f p)
