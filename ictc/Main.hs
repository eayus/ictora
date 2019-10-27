module Main where

import Core.Lang
import SpirV.CodeGen
import SpirV.Lang
import CodeGen.Convert
import CodeGen.State

main :: IO ()
main = do
    let st = codeGen $ cgFunc testCoreProg
    let instrs = instructions st
    putStrLn $ toAsm (SpirVProgram instrs)
    --mapM_ print (instructions st)
    --print (idMap st)
{--main = do
    let asm = evalState (exprToAsm testProgram) (NameStore 0)
    print asm--}
