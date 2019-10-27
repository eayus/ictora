module SpirV.Operations where

import SpirV.Options
import SpirV.CodeGen


-- Whether an operation returns a resulting ID
data OperationType = Result | NoResult

data Operation (t :: OperationType) where
    -- params: integer width (bits), signedness
    OpTypeInt :: Int -> Signedness -> Operation Result

    -- params: ids of aggregate types
    OpTypeStruct :: [Id] -> Operation Result

    -- params: return type, parameter types
    OpTypeFunction :: Id -> [Id] -> Operation Result

    -- params: storage class, type we're pointing to
    OpTypePointer :: StorageClass -> Id -> Operation Result

    -- params: pointer type, storage class, optional initial value
    OpVariable :: Id -> StorageClass -> Maybe Id -> Operation Result

    -- params: subtype pointer, struct pointer, structure indices
    OpAccessChain :: Id -> Id -> [Id] -> Operation Result

    -- params: type after dereference, pointer which will be dereferenced, memory access type
    OpLoad :: Id -> Id -> MemoryAccessType -> Operation Result

    -- params: variable pointer, value to write, memory access type
    OpStore :: Id -> Id -> MemoryAccessType -> Operation NoResult

    -- params: type of the constant, value
    OpConstant :: Id -> Literal -> Operation Result

    -- params: return type, function options, function type
    OpFunction :: Id -> FunctionOptions -> Id -> Operation Result

    -- params: parameter type
    OpFunctionParameter :: Id -> Operation Result

    -- params: value to return
    OpReturnValue :: Id -> Operation NoResult

    -- params: N/A
    OpFunctionEnd :: Operation NoResult

    -- params: return type, func to call, parameters to pass
    OpFunctionCall :: Id -> Id -> [Id] -> Operation Result

    -- params: capability to enable
    OpCapability :: Capability -> Operation NoResult

    -- params: addressing model, memory model
    OpMemoryModel :: AddressingModel -> MemoryModel -> Operation NoResult

    -- params: execution model, id of function (forward declared), unique name for func, list of
    -- global varialbes which are the input/outputs to the shader
    OpEntryPoint :: ExecutionModel -> Id -> String -> [Id] -> Operation NoResult



-- Assembly Generation

instance ToAsm (Operation Result) where
    toAsm (OpTypeInt width signedness) = unwords ["OpTypeInt", show width, toAsm signedness]

    toAsm (OpTypeStruct subTypes) = unwords $ "OpTypeStruct" : map toAsm subTypes

    toAsm (OpTypeFunction retType paramTypes) = unwords $ "OpTypeFunction" : toAsm retType : map toAsm paramTypes

    toAsm (OpTypePointer storage ptrType) = unwords ["OpTypePointer", toAsm storage, toAsm ptrType]

    toAsm (OpVariable ptrType storage (Just val)) = unwords ["OpVariable", toAsm ptrType, toAsm storage, toAsm val]
    toAsm (OpVariable ptrType storage Nothing) = unwords ["OpVariable", toAsm ptrType, toAsm storage]

    toAsm (OpLoad typ ptr accessType) = unwords ["OpLoad", toAsm typ, toAsm ptr, toAsm accessType]

    toAsm (OpConstant typ val) = unwords ["OpStore", toAsm typ, toAsm val]

    toAsm (OpFunction retType opts funcType) = unwords ["OpFunction", toAsm retType, toAsm opts, toAsm funcType]

    toAsm (OpFunctionParameter typ) = unwords ["OpFunctionParameter", toAsm typ]

    toAsm (OpFunctionCall retType func params) = unwords $ "OpFunctionCall" : toAsm retType : toAsm func : map toAsm params


instance ToAsm (Operation NoResult) where
    toAsm (OpStore varPtr val accessType) = unwords ["OpStore", toAsm varPtr, toAsm val, toAsm accessType]

    toAsm (OpReturnValue val) = unwords ["OpReturnValue", toAsm val]

    toAsm OpFunctionEnd = "OpFunctionEnd"

    toAsm (OpCapability cap) = unwords ["OpCapability", toAsm cap]

    toAsm (OpMemoryModel addrModel memModel) = unwords ["OpMemoryModel", toAsm addrModel, toAsm memModel]

    toAsm (OpEntryPoint execModel func name inOuts) = unwords $ "OpEntryPoint" : toAsm execModel : toAsm func : show name : map toAsm inOuts

