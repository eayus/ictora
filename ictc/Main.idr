module Main

import Ictora.GCore.Lang
import Ictora.GCore.ToSpirV
import SpirV.Raw.Output

main : IO ()
main = putStrLn . toAsm . convert $ testProg

