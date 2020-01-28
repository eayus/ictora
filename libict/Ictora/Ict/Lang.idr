module Ictora.Ict.Lang

import public Data.Vect
import public Ictora.Util
import public Ictora.Ict.Types

%access public export


Name : Type
Name = String


data IExpr : (locals : Vect n ITy) -> (globals : Vect m (Name, ITy)) -> ITy -> Type where
    ILocalVar : IndexIs i local ty -> IExpr local global ty
    IGlobalVar : LookupIs vname globals ty -> IExpr local global ty
    ILit : Int -> IExpr local global ITInt
    IApp : IExpr local global (a ~> b) -> IExpr local global a -> IExpr local global b
    ILam : IExpr (a :: local) global b -> IExpr local global (a ~> b)
    ILet : IExpr local global a -> IExpr (a :: local) global b -> IExpr local global b


data IProgram : (globals : Vect m (Name, ITy)) -> Type where
    IEmptyProg : IProgram globals
    IConsFunc : (fname : Name)
             -> IExpr [] globals ty
             -> IProgram ((fname, ty) :: globals)
             -> IProgram globals
