module Main

import SpirV.Typed.Builder
import SpirV.Typed.Type
import SpirV.Raw.Output
import SpirV.Raw.Options
import Control.Monad.State

codegen : Builder ()
codegen = do
    setCapabilities [ShaderCap, MatrixCap]
    setMemModel LogicalAddr SimpleMem

    let tInt = TInt 32 Unsigned
    function (MkFuncType tInt [(KScalar ** tInt), (KScalar ** tInt)]) (MkFunctionOptions Inline Pure)
    res <- var tInt FunctionStorage
    five <- constant tInt 5
    writeVar res five NormalMemAccess
    pure ()

main : IO ()
main = putStrLn $ toAsm $ runBuilder codegen

