module SpirV.Typed.Builder

import public Control.Monad.State
import public SpirV.Typed.Type
import public SpirV.Typed.TypeMap
import public SpirV.Raw.Program
import public SpirV.Raw.Operations
import public SpirV.Raw.Options
import public SpirV.Raw.Misc

%access public export

-- New

record Function where
    constructor MkFunction
    ident : Id
    type : FuncType
    params : List Id
    opts : FunctionOptions
    blocks : Code

record Module where
    constructor MkModule
    caps : List Capability
    memModel : MemoryModel
    addrModel : AddressingModel
    funcs : List Function
    mainIndex : Nat


record BuilderState where
    constructor MkBuilderState
    types : TypeMap
    code : List Instruction
    nextId : Int

Builder : Type -> Type
Builder = State BuilderState

initBS : BuilderState
initBS = MkBuilderState (MkTypeMap [] []) [] 0

freshId : Builder Id
freshId = do
    res <- nextId <$> get
    modify $ record { nextId $= (+ 1) }
    pure $ show res

addStaticInstr : Instruction -> Builder ()
addStaticInstr instr = modify $ record { BuilderState.code $= (++ [instr]) }

mutual
    cgType : VarType a -> Builder Id
    cgType vt = do
        res <- freshId
        case vt of
            (TInt width sign) => addStaticInstr $ MkInstrWithRes res $ OpTypeInt width sign
            (TFloat width) => addStaticInstr $ MkInstrWithRes res $ OpTypeFloat width
            (TStruct subTypes) => do
                subTypeIds <- traverse (\(_ ** subType) => getType subType) subTypes
                addStaticInstr $ MkInstrWithRes res $ OpTypeStruct subTypeIds
            TBool => addStaticInstr $ MkInstrWithRes res $ OpTypeBool
            (TPtr derefType) => do
                derefTypeId <- getType derefType
                addStaticInstr $ MkInstrWithRes res $ OpTypePointer FunctionStorage derefTypeId
        modify $ record { types $= insertType vt res }
        pure res

    getType : VarType a -> Builder Id
    getType t = do
        tm <- types <$> get
        case lookupType t tm of
            Just i => pure i
            Nothing => cgType t

cgFuncType : FuncType -> Builder Id
cgFuncType ft = do
    retTypeId <- getType (retType ft)
    paramTypeIds <- traverse (\(_ ** paramType) => getType paramType) (paramTypes ft)
    res <- freshId
    addStaticInstr $ MkInstrWithRes res $ OpTypeFunction retTypeId paramTypeIds
    modify $ record { types $= insertFuncType ft res }
    pure res

getFuncType : FuncType -> Builder Id
getFuncType ft = do
    tm <- types <$> get
    case lookupFuncType ft tm of
         Just i => pure i
         Nothing=> cgFuncType ft


functionToCode : Function -> Builder Code
functionToCode (MkFunction ident type params opts bodyCode) = do
    retTypeId <- getType (retType type)
    funcTypeId <- getFuncType type
    paramTypeIds <- traverse (\(_ ** t) => getType t) (paramTypes type)
    let x = MkInstrWithRes ident $ OpFunction retTypeId opts funcTypeId
    let f = \param => \paramType => MkInstrWithRes param $ OpFunctionParameter paramType
    let ys = zipWith f params paramTypeIds
    let z = MkInstr OpFunctionEnd

    pure $ [x] ++ ys ++ bodyCode ++ [z]

moduleToCode : Module -> Builder Code
moduleToCode (MkModule caps mem addr funcs i) = do
    let capsCode = MkInstr . OpCapability <$> caps
    let memCode = MkInstr $ OpMemoryModel addr mem
    funcCode <- traverse functionToCode funcs
    staticCode <- BuilderState.code <$> get

    pure $ capsCode ++ [memCode] ++ staticCode ++ concat funcCode

build : Builder Module -> Program
build builder = evalState (builder >>= moduleToCode) initBS
