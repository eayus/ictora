module Main where

import Syntax

main :: IO ()
main = do
    let prog = comp testProgram
    print (toAsm prog (0, []))
