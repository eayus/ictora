module Ictora.GCore.Lang

import public Ictora.GCore.Types
import Ictora.Util
import Data.List
import Data.Fin
import Data.Vect
import Data.HVect

%access public export

GIdentifier : Type
GIdentifier = String

GScope : Nat -> Type
GScope n = Vect n (GIdentifier, GTy)

mutual
    data GExpr : GVarTy -> (len : Nat) -> GScope len -> Type where
        GLit : {type : GVarTy} -> InterpTy type -> GExpr type len scope
        GVar : (name : GIdentifier) -> Elem (name, GTyVar type) scope -> GExpr type len scope
        GLet : (name : GIdentifier)
           -> GExpr varType len scope
           -> GExpr exprType (S len) ((name, GTyVar varType) :: scope)
           -> GExpr exprType len scope
        GFuncCall : (name : GIdentifier)
           -> Elem (name, GTyFunc (MkGFuncTy ret arity params)) scope
           -- -> HVect (map (\pt => GExpr pt len scope) params)
           -> GParamList len scope params
           -> GExpr ret len scope

    
    data GParamList : (len : Nat) -> GScope len -> Vect n GVarTy -> Type where
        EmptyParams : GParamList len scope []
        (::) : GExpr type len scope -> GParamList len scope types -> GParamList len scope (type :: types)


record GFunction (type : GFuncTy) (len : Nat) (scope : GScope len) where
    constructor MkGFunction
    name : GIdentifier
    paramNames : Vect (arity type) GIdentifier
    body : GExpr (ret type) (arity type + len) ((zip paramNames $ map GTyVar $ params type) ++ scope)

funcType : {ty : GFuncTy} -> GFunction ty _ _ -> GFuncTy
funcType {ty} _ = ty

data GProgram : (len : Nat) -> GScope len -> Type where
    Nil : GProgram len scope
    Cons : (f : GFunction type len scope) -> GProgram (S len) ((name f, GTyFunc type) :: scope) -> GProgram len scope


-- In the program, does a function this name and type exist?
data FuncExists : GProgram len scope -> GIdentifier -> GFuncTy -> Type where
    Here : FuncExists (Cons func program) (name func) (funcType func)
    There : FuncExists program name type -> FuncExists (Cons topFunc program) name type


record GCompleteProgram where
    constructor MkGCompleteProgram
    prog : GProgram 0 []
    vertProof : FuncExists prog "vert" (MkGFuncTy (GTVec Four) 0 [])
    fragProof : FuncExists prog "frag"  (MkGFuncTy (GTVec Four) 0 [])


numFuncs : GProgram _ _ -> Nat
numFuncs Nil = 0
numFuncs (Cons _ prog) = succ $ numFuncs prog


funcIndex : (prog : GProgram _ _) -> FuncExists prog name type -> Fin (numFuncs prog)
funcIndex Nil p impossible
funcIndex (Cons f fs) Here = 0
funcIndex (Cons f fs) (There p) = FS $ funcIndex fs p
