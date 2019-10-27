module SpirV.CodeGen where

type Asm = String

newtype Id = Id String

class ToAsm a where
    toAsm :: a -> Asm

instance ToAsm Id where
    toAsm (Id string) = '%' : string
