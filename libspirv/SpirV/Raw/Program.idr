module SpirV.Raw.Program

import SpirV.Raw.Operations
import SpirV.Raw.Misc

%access public export

data Instruction : Type where
    MkInstr : Operation NoResult -> Instruction
    MkInstrWithRes : Id -> Operation Result -> Instruction
    
Program : Type
Program = List Instruction
