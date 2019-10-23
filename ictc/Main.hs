module Main where

import Core.Lang
import CodeGen.Convert
import CodeGen.State

main :: IO ()
main = do
    let st = codeGen $ cgFunc testCoreProg
    mapM_ print (instructions st)
    --print (idMap st)
{--main = do
    let asm = evalState (exprToAsm testProgram) (NameStore 0)
    print asm--}
