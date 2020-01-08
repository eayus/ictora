module SpirV.Raw.Output

import SpirV.Raw.Program
import SpirV.Raw.Misc
import SpirV.Raw.Options
import SpirV.Raw.Operations

%access public export

interface ToAsm a where
    toAsm : a -> String


ToAsm Id where
    toAsm ident = "%" ++ ident

ToAsm Literal where
    toAsm (IntLit n) = show n
    toAsm (FloatLit n) = show n

ToAsm Signedness where
    toAsm Signed = "1"
    toAsm Unsigned = "0"

ToAsm ShouldInline where
    toAsm Inline = "Inline"
    toAsm NoInline = "DontInline"
    toAsm MaybeInline = "None"

ToAsm Purity where
    toAsm Impure = "None"
    toAsm Pure = "Pure"
    toAsm Const = "Const"

ToAsm FunctionOptions where
    toAsm opts = let
        s1 = toAsm $ shouldInline opts
        s2 = toAsm $ purity opts
        in s1 ++ "|" ++ s2

ToAsm StorageClass where
    toAsm FunctionStorage = "Function"
    toAsm InputStorage = "Input"
    toAsm OutputStorage = "Output"

ToAsm MemoryAccessType where
    toAsm NormalMemAccess = "None"

ToAsm Capability where
    toAsm ShaderCap = "Shader"
    toAsm MatrixCap = "Matrix"

ToAsm AddressingModel where
    toAsm LogicalAddr = "Logical"
    toAsm Physical32Addr = "Physical32"
    toAsm Physical64Addr = "Physical64"

ToAsm ExecutionModel where
    toAsm VertexShader = "Vertex"
    toAsm FragmentShader = "Fragment"

ToAsm MemoryModel where
    toAsm SimpleMem = "Simple"
    toAsm GLSL450Mem = "GLSL450"
    toAsm OpenCLMem = "OpenCL"

ToAsm (Operation a) where
    toAsm (OpTypeInt width sign) = unwords ["OpTypeInt", show width, toAsm sign]
    toAsm (OpTypeFloat width) = unwords ["OpTypeFloat", show width]
    toAsm OpTypeBool = "OpTypeBool"
    toAsm (OpTypeStruct ids) = unwords ("OpTypeStruct" :: (map toAsm ids))
    toAsm (OpTypeFunction ret param) = unwords $ "OpTypeFunction" :: toAsm ret :: map toAsm param
    toAsm (OpTypePointer sc deref) = unwords ["OpTypePointer", toAsm sc, toAsm deref]
    toAsm OpTypeVoid = "OpTypeVoid"
    toAsm (OpTypeVector columnType size) = unwords ["OpTypeVector", toAsm columnType, show size]
    toAsm (OpTypeArray elemType len) = unwords ["OpTypeArray", toAsm elemType, toAsm len]
    toAsm (OpVariable ptrType sc Nothing) = unwords ["OpVariable", toAsm ptrType, toAsm sc]
    toAsm (OpVariable ptrType sc (Just val)) = unwords ["OpVariable", toAsm ptrType, toAsm sc, toAsm val]
    toAsm (OpAccessChain ptrType struct indices) = unwords $ "OpAccessChain" :: toAsm struct :: map toAsm indices
    toAsm (OpLoad type ptr access) = unwords ["OpLoad", toAsm type, toAsm ptr, toAsm access]
    toAsm (OpStore var val access) = unwords ["OpStore", toAsm var, toAsm val, toAsm access]
    toAsm (OpConstant type lit) = unwords ["OpConstant", toAsm type, toAsm lit]
    toAsm (OpConstantComposite type fieldVals) = unwords $ "OpConstantComposite" :: toAsm type :: map toAsm fieldVals
    toAsm (OpConstantTrue type) = unwords ["OpConstantTrue", toAsm type]
    toAsm (OpConstantFalse type) = unwords ["OpConstantFalse", toAsm type]
    toAsm (OpFunction retType opts funcType) = unwords ["OpFunction", toAsm retType, toAsm opts, toAsm funcType]
    toAsm (OpFunctionParameter type) = unwords ["OpFunctionParameter", toAsm type]
    toAsm OpReturn = "OpReturn"
    toAsm (OpReturnValue val) = unwords ["OpReturnValue", toAsm val]
    toAsm OpFunctionEnd = "OpFunctionEnd"
    toAsm (OpFunctionCall ret func param) = unwords $ "OpFunctionCall" :: toAsm ret :: toAsm func :: map toAsm param
    toAsm (OpCapability cap) = unwords ["OpCapability", toAsm cap]
    toAsm (OpMemoryModel addr mem) = unwords ["OpMemoryModel", toAsm addr, toAsm mem]
    toAsm (OpEntryPoint exec func name inouts) = unwords $ "OpEntryPoint" :: toAsm exec :: toAsm func :: show name :: map toAsm inouts
    toAsm OpLabel = "OpLabel"
    toAsm (OpIAdd type lhs rhs) = unwords ["OpIAdd", toAsm type, toAsm lhs, toAsm rhs]
    toAsm (OpFAdd type lhs rhs) = unwords ["OpFAdd", toAsm type, toAsm lhs, toAsm rhs]
    toAsm (OpISub type lhs rhs) = unwords ["OpISub", toAsm type, toAsm lhs, toAsm rhs]
    toAsm (OpFSub type lhs rhs) = unwords ["OpFSub", toAsm type, toAsm lhs, toAsm rhs]
    toAsm (OpLogicalNot type operand) = unwords ["OpLogicalNot", toAsm type, toAsm operand]
    toAsm (OpSelect type cond ifTrue ifFalse) = unwords $ "OpSelect" :: map toAsm [type, cond, ifTrue, ifFalse]

ToAsm Instruction where
    toAsm (MkInstr op) = toAsm op
    toAsm (MkInstrWithRes ident op) = toAsm ident ++ " = " ++ toAsm op

ToAsm Program where
    toAsm prog = unlines $ map toAsm prog

