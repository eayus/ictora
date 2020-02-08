module Ictora.G.Lang

import public Data.Vect
import public Data.Fin
import public Ictora.G.Types
import public Ictora.Util


%access public export


mutual
    data ParamList : (locals : Vect n GTy) -> (globals : Vect m GFuncTy) -> Vect arity GTy -> Type where
        Nil : ParamList locals globals []
        (::) : GExpr locals globals t
            -> ParamList locals globals ts
            -> ParamList locals globals (t :: ts)


    data GExpr : (locals : Vect n GTy) -> (globals : Vect m GFuncTy) -> GTy -> Type where
        GLocalVar : IndexIs i locals ty -> GExpr locals globals ty
        GLit : Int -> GExpr locals globals GTInt
        GLet : GExpr locals globals a -> GExpr (a :: locals) globals b -> GExpr locals globals b
        GFuncCall : IndexIs i globals (MkGFuncTy paramTys resTy)
                 -> ParamList locals globals paramTys
                 -> GExpr locals globals resTy

    
data GFunction : (globals : Vect m GFuncTy) -> GFuncTy -> Type where
    MkGFunction : GExpr paramTys globals resTy -> GFunction globals (MkGFuncTy paramTys resTy)


data GProg : (globals : Vect m GFuncTy) -> Type where
    GEmptyProg : GProg []
    GConsFunc : GFunction globals funcTy
             -> GProg (funcTy :: globals)
             -> GProg globals
