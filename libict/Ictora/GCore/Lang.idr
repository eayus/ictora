module Ictora.GCore.Lang

import public Ictora.GCore.Type

%access public export

Identifier : Type
Identifier = String

data GLiteral = GLInt Int | GLBool Bool | GLFloat Double

mutual
    data GPrimitiveOp
        = GAdd GExpr GExpr
        | GFAdd GExpr GExpr
        | GSub GExpr GExpr
        | GFSub GExpr GExpr
        | GNot GExpr

    data GExpr
        = GLit GLiteral
        | GVar Identifier
        | GFuncCall Identifier (List GExpr) GExprType 
        | GIf GExpr GExpr GExpr GExprType
        | GPrimOp GPrimitiveOp
        | GLet Identifier GExpr GExpr


record GFunction where
    constructor MkGFunction
    type : GFuncType
    name : Identifier
    params : List Identifier
    body : GExpr


GProgram : Type
GProgram = List GFunction


testProg : GProgram
testProg = [ MkGFunction (MkGFuncType GTBool []) "vert" [] (GLit (GLBool True)) 
           , MkGFunction (MkGFuncType GTInt []) "frag" [] (GLit (GLInt 9)) ]
