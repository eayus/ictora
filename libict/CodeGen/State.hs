module CodeGen.State where

import Control.Monad.State.Lazy

import Core.Type
import Core.Lang
import SpirV.Lang
import SpirV.CodeGen
import SpirV.Options (StorageClass)

-- This module contains the types which hold state during code generation. This
-- mainly encompasses the mapping of 'Core' language identifiers to 'Spir-V'
-- identifiers.

data IdMap = IdMap
    { varTypes :: [(CType, Id)]
    , ptrTypes :: [((CType, StorageClass), Id)]
    , funcTypes :: [(FuncType, Id)]
    , functions :: [(FuncIdentifier, Id)] }

data IdGenerator = IdGenerator Int -- Uses an internal integer counter to generate fresh Ids

data CGState = CGState
    { idMap :: IdMap
    , instructions :: [Instruction]
    , idGen :: IdGenerator }

type CodeGen = State CGState

type LocalScope = [(VarIdentifier, Id)]

initialCGState :: CGState
initialCGState = CGState (IdMap [] [] [] []) [] (IdGenerator 0)

codeGen :: CodeGen a -> CGState
codeGen st = execState st initialCGState 

appendInstr :: Instruction -> CodeGen ()
appendInstr instr = do
    st <- get
    put $ st { instructions = instructions st ++ [instr] }

genId :: IdGenerator -> (IdGenerator, Id)
genId (IdGenerator n) = (IdGenerator (n + 1), Id $ show n)

freshId :: CodeGen Id
freshId = do
    st <- get
    let (newIdGen, newId) = genId $ idGen st
    put $ st { idGen = newIdGen }
    return newId

lookupVarType :: CType -> CodeGen (Maybe Id)
lookupVarType typ = (lookup typ) . varTypes . idMap <$> get

lookupPtrType :: CType -> StorageClass -> CodeGen (Maybe Id)
lookupPtrType typ sc = (lookup (typ, sc)) . ptrTypes . idMap <$> get

lookupFuncType :: FuncType -> CodeGen (Maybe Id)
lookupFuncType typ = (lookup typ) . funcTypes . idMap <$> get

lookupFunction :: FuncIdentifier -> CodeGen (Maybe Id)
lookupFunction func = (lookup func) . functions . idMap <$> get


insertVarType :: Id -> CType -> CodeGen ()
insertVarType typId typ = modify $ \st -> st { idMap = (idMap st) { varTypes = (typ, typId) : varTypes (idMap st) } }

insertPtrType :: Id -> CType -> StorageClass -> CodeGen ()
insertPtrType typId typ sc = modify $ \st -> st { idMap = (idMap st) { ptrTypes = ((typ, sc), typId) : ptrTypes (idMap st) } }

insertFuncType :: Id -> FuncType -> CodeGen ()
insertFuncType typId typ = modify $ \st -> st { idMap = (idMap st) { funcTypes = (typ, typId) : funcTypes (idMap st) } }

insertFunction :: Id -> FuncIdentifier -> CodeGen ()
insertFunction funcId func = modify $ \st -> st { idMap = (idMap st) { functions = (func, funcId) : functions (idMap st) } }
