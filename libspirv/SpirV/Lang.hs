module SpirV.Lang where

import SpirV.Operations
import SpirV.CodeGen

data Instruction where
    Instruction :: Operation NoResult -> Instruction
    InstructionWithResult :: Id -> Operation Result -> Instruction

newtype SpirVProgram = SpirVProgram [Instruction]


instance ToAsm Instruction where
    toAsm (Instruction op) = toAsm op
    toAsm (InstructionWithResult resId op) = toAsm resId ++ " = " ++ toAsm op

instance ToAsm SpirVProgram where
    toAsm (SpirVProgram instrs) = unlines $ map toAsm instrs
