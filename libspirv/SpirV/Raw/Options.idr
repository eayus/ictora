module SpirV.Raw.Options

%access public export

-- Unsigned vs Signed integers
data Signedness = Signed | Unsigned

-- Whether a function should be inlined. 'MaybeInline' lets SpirV compiler decide
data ShouldInline = Inline | NoInline | MaybeInline

-- Mathematical purity of a function. Pure functions can read global memory and dereference function parameters, const cannot.
data Purity = Impure | Pure | Const

-- Collection of options needed to define a function
record FunctionOptions where
    constructor MkFunctionOptions    
    shouldInline : ShouldInline
    purity : Purity

-- Whether a variable is local to the function, or a shader input/output variable
data StorageClass = FunctionStorage | InputStorage | OutputStorage

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


data ExecutionMode = OriginLowerLeft




Eq Signedness where
    Signed == Signed = True
    Unsigned == Unsigned = True
    _ == _ = False

Eq StorageClass where
    FunctionStorage == FunctionStorage = True
    InputStorage == InputStorage = True
    OutputStorage == OutputStorage = True
    _ == _ = False

