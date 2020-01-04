module Ictora.GCore.ToSpirV

import Ictora.GCore.Lang
import SpirV.Typed.Builder
import SpirV.Raw.Program
import Data.Vect


buildFunction : GFunction _ _ -> Function

buildFunctions : (prog : GProgram scope) -> Vect (numFuncs prog) Function
buildFunctions Nil = []
buildFunctions (Cons f prog) = buildFunction f :: buildFunctions prog


buildModule : GCompleteProgram -> Builder Module
buildModule (MkGCompleteProgram prog vertProof fragProof) = do
    let caps = [ShaderCap, MatrixCap]

    let sfuncs = buildFunctions prog

    let vertIndex = funcIndex prog vertProof
    let fragIndex = funcIndex prog fragProof

    pure $ MkModule caps SimpleMem LogicalAddr (numFuncs prog) sfuncs vertIndex fragIndex


{--
convert : GCompleteProgram -> Program
convert = build . buildModule--}
