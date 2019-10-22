module Main where

import Syntax
import Control.Monad.State.Lazy

main :: IO ()
main = do
    let asm = evalState (exprToAsm testProgram) (NameStore 0)
    print asm
