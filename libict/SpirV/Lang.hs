module SpirV.Lang where

newtype Id = Id String

data Signedness = Signed | Unsigned
data OperationType = Ret | NoRet -- Operartions eitehr return something, or are just a statement
data ShouldInline = Inline | DontInline | MaybeInline  -- MaybeInline lets SPIR-V compiler decide whether to inline
data Purity = Impure | Pure | Const   -- Pure functions can access global memory and read dereferenced func params, const cannot
data FunctionControl = FunctionControl 
    { inline :: ShouldInline
	, purity :: Purity }

data Operation (t :: OperationType) where
    OpTypeInt :: Int -> Signedness -> Operation Ret                  -- width (bits), sign
    OpTypeStruct :: [Id] -> Operation Ret                            -- ids of aggregate types
    OpTypeFunction :: Id -> [Id] -> Operation Ret                    -- result, return type, ids of parameter types
	OpFunction :: Id -> FunctionControl -> Id -> Operation Ret       -- return type, function control, function type
	OpFunctionParameter :: Id -> Operation Ret                       -- param type


data Instruction where
    InstrRet :: Id -> Operation Ret -> Instruction
    InstrNoRet :: Operation NoRet -> Instruction


instance Show Id where
    show (Id string) = '%' : string

instance Show Signedness where
    show Signed = "1"
    show Unsigned = "2"

instance Show ShouldInline where
    show Inline = "Inline"
	show DontInline "DontInline"
	show MaybeInline "None"

instance Show Purity where
    show Impure = "None"
	show Pure = "Pure"
	show Const = "Const"

instance Show FunctionControl where
    show (FunctionControl inl puri) = "(" ++ show inl ++ "|" ++ show puri ++ ")"

instance Show (Operation Ret) where
    show (OpTypeInt width sign) = "OpTypeInt " ++ show width ++ " " ++ show sign
    show (OpTypeStruct subTypes) = "OpTypeStruct " ++ unwords (show <$> subTypes)
    show (OpTypeFunction retType paramTypes) = "OpTypeFunction " ++ show retType ++ " " ++ unwords (show <$> paramTypes)

instance Show (Operation NoRet) where
    show _ = ""

instance Show Instruction where
    show (InstrRet res op) = show res ++ " = " ++ show op
    show (InstrNoRet op) = show op
