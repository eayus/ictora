module IR where

import Util
import qualified Syntax as S

data FuncDef = FuncDef Identifier [Identifier] [(Identifier, PrimitiveOp)] Identifier

data PrimitiveOp
    = Add Identifier Identifier -- Ints represent index into the array of primitveops 
    | FuncApp Identifier [Identifier]

type Program = [FuncDef]

instance Show FuncDef where
    show (FuncDef name params lets body) = name ++ paramString ++ "= \n" ++ concat letStrings ++ bodyString
        where letStrings = map (\(var, expr) -> "    let " ++ var ++ " = " ++ show expr ++ "\n") lets
              bodyString = "    return " ++ show body
              paramString = concat $ map (++ " ") params

instance Show PrimitiveOp where
    show (Add name1 name2) = name1 ++ " + " ++ name2
    show (FuncApp f args) = concat $ map (++ " ") args


testIR :: FuncDef
testIR = FuncDef "f" ["x", "y", "z"] [("tmp1", Add "x" "y"), ("tmp2", Add "z" "tmp1")] "tmp2"
