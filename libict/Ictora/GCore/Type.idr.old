module Ictora.GCore.Type

%access public export

data GExprType
    = GTInt
    | GTBool
    | GTFloat
    | GTSum (List GExprType)

GTUnit : GExprType
GTUnit = GTSum []


record GFuncType where
    constructor MkGFuncType
    ret : GExprType
    params : List GExprType
