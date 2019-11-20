module SpirV.Typed.Builder

import Control.Monad.State
import SpirV.Typed.Type
import SpirV.Typed.TypeMap
import SpirV.Raw.Program
import SpirV.Raw.Operations
import SpirV.Raw.Options
import SpirV.Raw.Misc

record BuilderState where
    constructor MkBS
    typeDefs : List Instruction
    typeMap : TypeMap
    code : List Instruction
    nextId : Int

Builder : Type -> Type
Builder = State BuilderState

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


cgType : VarType a -> Builder Id
cgType vt = do
    res <- case vt of
        (TInt width sign) => addOp $ OpTypeInt width sign
        (TFloat width) => addOp $ OpTypeFloat width
    modify $ record { typeMap $= insertType vt res }
    pure res

getType : VarType a -> Builder Id
getType t = do
    tm <- typeMap <$> get
    case lookupType t tm of
        Just i => pure i
        Nothing => cgType t


-- External Interface

constant : (t : VarType KScalar) -> IdrisVarType t -> Builder Id
constant type val = do
    typeId <- getType type
    addTypeOp $ OpConstant typeId $ scalarToLit type val
