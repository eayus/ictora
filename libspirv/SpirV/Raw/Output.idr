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
    toAsm Signed = "Signed"
    toAsm Unsigned = "Unsigned"

ToAsm ShouldInline where
    toAsm Inline = "Inline"
    toAsm NoInline = "DontInline"

ToAsm Purity where
    toAsm Impure = "None"
    toAsm Pure = "Pure"
    toAsm Const = "Const"

ToAsm FunctionOptions where
    toAsm opts = toAsm (shouldInline opts) ++ " | " ++ toAsm (purity opts)

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
    toAsm (OpTypeStruct ids) = unwords ("OpTypeStruct" :: (map toAsm ids))
    toAsm (OpTypeFunction ret param) = unwords $ "OpTypeFunction" :: toAsm ret :: map toAsm param
    toAsm (OpTypePointer sc deref) = unwords ["OpTypePointer", toAsm sc, toAsm deref]
    toAsm (OpVariable ptrType sc Nothing) = unwords ["OpVariable", toAsm ptrType, toAsm sc]
    toAsm (OpVariable ptrType sc (Just val)) = unwords ["OpVariable", toAsm ptrType, toAsm sc, toAsm val]
    toAsm (OpAccessChain ptrType struct indices) = unwords $ "OpAccessChain" :: toAsm struct :: map toAsm indices
    toAsm (OpLoad type ptr access) = unwords ["OpLoad", toAsm type, toAsm ptr, toAsm access]
    toAsm (OpStore var val access) = unwords ["OpStore", toAsm var, toAsm val, toAsm access]
    toAsm (OpConstant type lit) = unwords ["OpConstant", toAsm type, toAsm lit]
    toAsm (OpFunction retType opts funcType) = unwords ["OpFunction", toAsm retType, toAsm opts, toAsm funcType]
    toAsm (OpFunctionParameter type) = unwords ["OpFunctionParameter", toAsm type]
    toAsm (OpReturnValue val) = unwords ["OpReturnValue", toAsm val]
    toAsm OpFunctionEnd = "OpFunctionEnd"
    toAsm (OpFunctionCall ret func param) = unwords $ "OpFunctionCall" :: toAsm ret :: toAsm func :: map toAsm param
    toAsm (OpCapability cap) = unwords ["OpCapability", toAsm cap]
    toAsm (OpMemoryModel addr mem) = unwords ["OpMemoryModel", toAsm addr, toAsm mem]
    toAsm (OpEntryPoint exec func name inouts) = unwords $ "OpEntryPoint" :: toAsm exec :: toAsm func :: toAsm name :: map toAsm inouts

ToAsm Instruction where
    toAsm (MkInstr op) = toAsm op
    toAsm (MkInstrWithRes ident op) = toAsm ident ++ " = " ++ toAsm op

ToAsm Program where
    toAsm prog = unlines $ map toAsm prog

