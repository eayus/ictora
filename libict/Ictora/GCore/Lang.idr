module Ictora.GCore.Lang

import public Ictora.GCore.Types
import Ictora.Util
import Data.List
import Data.Fin

%access public export

GIdentifier : Type
GIdentifier = String

GScope : Type
GScope = Assoc GIdentifier GTy

data GExpr : GVarTy -> GScope -> Type where
    GLit : interpTy type -> GExpr type scope
    GVar : (name : GIdentifier) -> Elem (name, GTyVar type) scope -> GExpr type scope
    GLet : (name : GIdentifier)
       -> GExpr varType scope
       -> GExpr exprType ((name, GTyVar varType) :: scope)
       -> GExpr exprType scope

record GFunction (type : GFuncTy) (scope : GScope) where
    constructor MkGFunction
    name : GIdentifier
    body : GExpr (ret type) scope

funcType : {ty : GFuncTy} -> GFunction ty _ -> GFuncTy
funcType {ty} _ = ty

data GProgram : GScope -> Type where
    Nil : GProgram scope
    Cons : (f : GFunction type scope) -> GProgram ((name f, GTyFunc type) :: scope) -> GProgram scope


-- In the program, does a function this name and type exist?
data FuncExists : GProgram scope -> GIdentifier -> GFuncTy -> Type where
    Here : FuncExists (Cons func program) (name func) (funcType func)
    There : FuncExists program name type -> FuncExists (Cons topFunc program) name type


record GCompleteProgram where
    constructor MkGCompleteProgram
    prog : GProgram []
    vertProof : FuncExists prog "vert" (MkGFuncTy GTInt [GTInt])
    fragProof : FuncExists prog "frag" (MkGFuncTy GTInt [GTInt])


numFuncs : GProgram _ -> Nat
numFuncs Nil = 0
numFuncs (Cons _ prog) = succ $ numFuncs prog


funcIndex : (prog : GProgram _) -> FuncExists prog name type -> Fin (numFuncs prog)
funcIndex Nil p impossible
funcIndex (Cons f fs) Here = 0
funcIndex (Cons f fs) (There p) = FS $ funcIndex fs p
