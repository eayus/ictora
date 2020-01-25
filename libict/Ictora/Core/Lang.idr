module Ictora.Core.Lang

import public Data.Fin
import public Data.Vect
import public Ictora.Core.Types
import public Ictora.Util

%access public export


-- Variable i in the scope has type t
data HasType : (i : Fin len) -> Vect len CTy -> CTy -> Type where
    Here : HasType FZ (t :: _) t
    There : HasType i scope t -> HasType (FS i) (_ :: scope) t


data CExpr : Vect n CTy -> CTy -> Type where
    CLit : {litType : LitType ty} -> interpCTy litType -> CExpr scope ty
    CApp : CExpr scope (a ~> b) -> CExpr scope a -> CExpr scope b
    CLam : CExpr (a :: scope) b -> CExpr scope (a ~> b)
    CVar : HasType i scope a -> CExpr scope a
    CLet : CExpr scope a -> CExpr (a :: scope) b -> CExpr scope b


data CProgram : Vect n CTy -> Type where
    CEmpty : CProgram scope
    CConsFunc : CExpr scope a -> CProgram (a :: scope) -> CProgram scope
