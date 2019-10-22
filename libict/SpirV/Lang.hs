module SpirV.Lang where

newtype Id = Id String

data Signedness = Signed | Unsigned
data OperationType = Ret | NoRet -- Operartions eitehr return something, or are just a statement

data Operation (t :: OperationType) where
    OpTypeInt :: Int -> Signedness -> Operation Ret           -- width (bits), sign
    OpTypeStruct :: [Id] -> Operation Ret                     -- ids of aggregate types
    OpTypeFunction :: Id -> [Id] -> Operation Ret             -- result, return type, ids of parameter types


data Instruction where
    InstrRet :: Id -> Operation Ret -> Instruction
    InstrNoRet :: Operation NoRet -> Instruction


instance Show Id where
    show (Id string) = '%' : string

instance Show Signedness where
    show Signed = "1"
    show Unsigned = "2"

instance Show (Operation Ret) where
    show (OpTypeInt width sign) = "OpTypeInt " ++ show width ++ " " ++ show sign
    show (OpTypeStruct subTypes) = "OpTypeStruct " ++ unwords (show <$> subTypes)
    show (OpTypeFunction retType paramTypes) = "OpTypeFunction " ++ show retType ++ " " ++ unwords (show <$> paramTypes)

instance Show (Operation NoRet) where
    show _ = ""

instance Show Instruction where
    show (InstrRet res op) = show res ++ " = " ++ show op
    show (InstrNoRet op) = show op
