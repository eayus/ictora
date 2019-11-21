module SpirV.Typed.Builder

import Control.Monad.State
import SpirV.Typed.Type
import SpirV.Typed.TypeMap
import SpirV.Raw.Program
import SpirV.Raw.Operations
import SpirV.Raw.Options
import SpirV.Raw.Misc

%access public export

record BuilderState where
    constructor MkBS
    typeDefs : List Instruction
    code : List Instruction
    typeMap : TypeMap
    nextId : Int

Builder : Type -> Type
Builder = State BuilderState

initBS : BuilderState
initBS = MkBS [] [] (MkTypeMap [] []) 0

freshId : Builder Id
freshId = do
    res <- nextId <$> get
    modify $ record { nextId $= (+ 1) }
    pure $ show res

resType : OperationType -> Type
resType Result = Id
resType NoResult = ()

addInstr : Instruction -> BuilderState -> BuilderState
addInstr instr = record { code $= (instr ::) }

addTypeInstr : Instruction -> BuilderState -> BuilderState
addTypeInstr instr = record { typeDefs $= (instr ::) }

addOp : {a : OperationType} -> Operation a -> Builder (resType a)
addOp {a = NoResult} op = modify $ addInstr $ MkInstr op
addOp {a = Result} op = do
    newId <- freshId
    modify $ addInstr $ MkInstrWithRes newId op
    pure newId

addTypeOp : {a : OperationType} -> Operation a -> Builder (resType a)
addTypeOp {a = NoResult} op = modify $ addTypeInstr $ MkInstr op
addTypeOp {a = Result} op = do
    newId <- freshId
    modify $ addTypeInstr $ MkInstrWithRes newId op
    pure newId


mutual
    cgType : VarType a -> Builder Id
    cgType vt = do
        res <- case vt of
            (TInt width sign) => addTypeOp $ OpTypeInt width sign
            (TFloat width) => addTypeOp $ OpTypeFloat width
            (TStruct subTypes) => do
                subTypeIds <- traverse (\(_ ** subType) => getType subType) subTypes
                addTypeOp $ OpTypeStruct subTypeIds
            (TPtr derefType) => do
                derefTypeId <- getType derefType
                addTypeOp $ OpTypePointer FunctionStorage derefTypeId
        modify $ record { typeMap $= insertType vt res }
        pure res

    getType : VarType a -> Builder Id
    getType t = do
        tm <- typeMap <$> get
        case lookupType t tm of
            Just i => pure i
            Nothing => cgType t

cgFuncType : FuncType -> Builder Id
cgFuncType ft = do
    retTypeId <- getType (retType ft)
    paramTypeIds <- traverse (\(_ ** paramType) => getType paramType) (paramTypes ft)
    res <- addTypeOp $ OpTypeFunction retTypeId paramTypeIds
    modify $ record { typeMap $= insertFuncType ft res }
    pure res

getFuncType : FuncType -> Builder Id
getFuncType ft = do
    tm <- typeMap <$> get
    case lookupFuncType ft tm of
         Just i => pure i
         Nothing=> cgFuncType ft

-- External Interface

data Variable = MkVariable Id (VarType t)

data Value = MkValue Id


runBuilder : Builder a -> Program
runBuilder builder = let (MkBS types code _ _) = execState builder initBS in (reverse types) ++ (reverse code)

setCapabilities : List Capability -> Builder ()
setCapabilities = ignore . traverse (addOp . OpCapability)

setMemModel : AddressingModel -> MemoryModel -> Builder ()
setMemModel addr mem = addOp $ OpMemoryModel addr mem

functionWithId : FuncType -> FunctionOptions -> Id -> Builder Id
functionWithId ft opts ident = do
    retTypeId <- getType $ retType ft
    funcTypeId <- getFuncType ft
    modify $ addInstr $ MkInstrWithRes ident $ OpFunction retTypeId opts funcTypeId
    pure ident

function : FuncType -> FunctionOptions -> Builder Id
function ft opts = freshId >>= functionWithId ft opts

constant : (t : VarType KScalar) -> IdrisVarType t -> Builder Value
constant type val = do
    typeId <- getType type
    ident <- addOp $ OpConstant typeId $ scalarToLit type val
    pure $ MkValue ident

var : VarType t -> StorageClass -> Builder Variable
var vt sc = do
    ptrTypeId <- getType $ TPtr vt
    ident <- addOp $ OpVariable ptrTypeId sc Nothing
    pure $ MkVariable ident vt

readVar : Variable -> MemoryAccessType -> Builder Value
readVar (MkVariable ident vt) access = do
    typeId <- getType vt
    ident <- addOp $ OpLoad typeId ident access
    pure $ MkValue ident

writeVar : Variable -> Value -> MemoryAccessType -> Builder ()
writeVar (MkVariable var _) (MkValue val) access = addOp $ OpStore var val access

fieldVar : Variable -> Nat -> Builder Variable
fieldVar (MkVariable var (TStruct subTypes)) n = do
    let (Just (_ ** subType)) = index' n subTypes
    (MkValue index) <- constant (TInt 32 Unsigned) n
    subTypePtrId <- getType $ TPtr subType
    ident <- addOp $ OpAccessChain subTypePtrId var [index]
    pure $ MkVariable ident subType
