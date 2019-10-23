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
    OpReturnValue :: Id -> Operation NoRet                           -- the value to return
    OpFunctionEnd :: Operation NoRet
    OpConstant :: Show a => Id -> a -> Operation Ret                 -- type, literal
    OpFunctionCall :: Id -> Id -> [Id] -> Operation Ret              -- return type, func to call, parameters


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
    show DontInline = "DontInline"
    show MaybeInline = "None"

instance Show Purity where
    show Impure = "None"
    show Pure = "Pure"
    show Const = "Const"

instance Show FunctionControl where
    show (FunctionControl inl puri) = "(" ++ show inl ++ "|" ++ show puri ++ ")"

instance Show (Operation Ret) where
    show (OpTypeInt width sign) = unwords ["OpTypeInt", show width, show sign]
    show (OpTypeStruct subTypes) = unwords $ "OpTypeStruct" : map show subTypes
    show (OpTypeFunction retType paramTypes) = unwords $ "OpTypeFunction" : show retType : map show paramTypes
    show (OpFunction ret cntrl typ) = unwords ["OpFunction", show ret, show cntrl, show typ]
    show (OpFunctionParameter typ) = unwords ["OpFunctionParameter", show typ]
    show (OpConstant typ val) = unwords ["OpConstant", show typ, show val]
    show (OpFunctionCall ret f params) = unwords $ "OpFunctionCall" : show ret : show f : map show params

instance Show (Operation NoRet) where
    show OpFunctionEnd = "OpFunctionEnd"
    show (OpReturnValue ret) = unwords ["OpReturnValue", show ret]

instance Show Instruction where
    show (InstrRet res op) = show res ++ " = " ++ show op
    show (InstrNoRet op) = show op
