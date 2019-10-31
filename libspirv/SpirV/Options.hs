module SpirV.Options where

import SpirV.CodeGen

-- A raw literal
data Literal = IntLit Int

-- Unsigned vs Signed integers
data Signedness = Signed | Unsigned

-- Whether a function should be inlined. 'MaybeInline' lets SpirV compiler decide
data ShouldInline = Inline | NoInline | MaybeInline

-- Mathematical purity of a function. Pure functions can read global memory and dereference function parameters, const cannot.
data Purity = Impure | Pure | Const

-- Collection of options needed to define a function
data FunctionOptions = FunctionOptions
    { shouldInline :: ShouldInline
    , purity :: Purity }

-- Whether a variable is local to the function, or a shader input/output variable
data StorageClass = FunctionStorage | InputStorage | OutputStorage deriving Eq

-- Properties of a memory access (volatility etc.). Only the default memory access type is supported currently.
data MemoryAccessType = NormalMemAccess

-- A SpirV capability. Used to enable a "feature" that can be used in the program
data Capability = ShaderCap | MatrixCap

-- Addressing Model
data AddressingModel = LogicalAddr | Physical32Addr | Physical64Addr

-- The type of the entry point, whether it is a shader (vertex, fragment, tesselation), computer kernel etc.
data ExecutionModel = VertexShader | FragmentShader

-- Memory Model
data MemoryModel = SimpleMem | GLSL450Mem | OpenCLMem



-- Assembly Generation

instance ToAsm Literal where
    toAsm (IntLit x) = show x

instance ToAsm Signedness where
    toAsm Signed = "1"
    toAsm Unsigned = "2"

instance ToAsm ShouldInline where
    toAsm Inline = "Inline"
    toAsm NoInline = "DontInline"
    toAsm MaybeInline = "None"

instance ToAsm Purity where
    toAsm Impure = "None"
    toAsm Pure = "Pure"
    toAsm Const = "Const"

instance ToAsm FunctionOptions where
    toAsm opts = "(" ++ toAsm (shouldInline opts) ++ "|" ++ toAsm (purity opts) ++ ")"

instance ToAsm StorageClass where
    toAsm FunctionStorage = "Function"
    toAsm InputStorage = "Input"
    toAsm OutputStorage = "Output"

instance ToAsm MemoryAccessType where
    toAsm NormalMemAccess = "None"

instance ToAsm Capability where
    toAsm ShaderCap = "Shader"
    toAsm MatrixCap = "Matrix"

instance ToAsm AddressingModel where
    toAsm LogicalAddr = "Logical"
    toAsm Physical32Addr = "Physical32"
    toAsm Physical64Addr = "Physical64"

instance ToAsm ExecutionModel where
    toAsm VertexShader = "Vertex"
    toAsm FragmentShader = "Fragment"

instance ToAsm MemoryModel where
    toAsm SimpleMem = "Simple"
    toAsm GLSL450Mem = "GLSL450"
    toAsm OpenCLMem = "OpenCL"

