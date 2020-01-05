module SpirV.Raw.Operations

import SpirV.Raw.Options
import SpirV.Raw.Misc

%access public export

-- Whether an operation returns a resulting ID
data OperationType = Result | NoResult

data Operation : OperationType -> Type where
    OpTypeInt : (width : Nat) -> Signedness -> Operation Result

    OpTypeFloat : (width : Nat) -> Operation Result

    OpTypeBool : Operation Result

    OpTypeStruct : (types : List Id) -> Operation Result

    OpTypeFunction : (returnType : Id) -> (paramTypes : List Id) -> Operation Result

    OpTypePointer : StorageClass -> (derefType : Id) -> Operation Result

    OpTypeVoid : Operation Result

    OpVariable : (ptrType : Id) -> StorageClass -> (initVal : Maybe Id) -> Operation Result

    OpAccessChain : (ptrSubType : Id) -> (struct : Id) -> (fieldIndices : List Id) -> Operation Result

    OpLoad : (derefType : Id) -> (ptr : Id) -> MemoryAccessType -> Operation Result

    OpStore : (var : Id) -> (val : Id) -> MemoryAccessType -> Operation NoResult

    OpConstant : (type : Id) -> Literal -> Operation Result

    OpConstantTrue : (type : Id) -> Operation Result

    OpConstantFalse : (type : Id) -> Operation Result

    OpConstantComposite : (type : Id) -> (fieldVals : List Id) -> Operation Result

    OpFunction : (retType : Id) -> FunctionOptions -> (funcType : Id) -> Operation Result

    OpFunctionParameter : (type : Id) -> Operation Result

    OpReturn : Operation NoResult

    OpReturnValue : (val : Id) -> Operation NoResult

    OpFunctionEnd : Operation NoResult

    OpFunctionCall : (retType : Id) -> (func : Id) -> (params : List Id) -> Operation Result

    OpCapability : Capability -> Operation NoResult

    OpMemoryModel : AddressingModel -> MemoryModel -> Operation NoResult

    OpEntryPoint : ExecutionModel -> (func : Id) -> (name : String) -> (inOutVars : List Id) -> Operation NoResult

    OpLabel : Operation Result

    OpIAdd : (type : Id) -> (lhs : Id) -> (rhs : Id) -> Operation Result

    OpISub : (type : Id) -> (lhs : Id) -> (rhs : Id) -> Operation Result

    OpFAdd : (type : Id) -> (lhs : Id) -> (rhs : Id) -> Operation Result

    OpFSub: (type : Id) -> (lhs : Id) -> (rhs : Id) -> Operation Result

    OpLogicalNot : (type : Id) -> (operand : Id) -> Operation Result

    OpSelect : (type : Id) -> (cond : Id) -> (ifTrue : Id) -> (ifFalse : Id) -> Operation Result
