module CodeGen.State where

import prelude hiding (lookup)

import Control.Monad.State.Lazy
import qualified Data.HashMap.Strict

import Core.Lang
import SpirV.Lang
import SpirV.Type
import SpirV.CodeGen
import SpirV.Options (StorageClass)

-- This module contains the types which hold state during code generation. This
-- mainly encompasses the mapping of 'Core' language identifiers to 'Spir-V'
-- identifiers.

data IdMap = IdMap
    { varTypes :: HashMap VarType Id
    , funcTypes :: HashMap FuncType Id
    , functions :: HashMap FuncIdentifier Id }

data IdGenerator = IdGenerator Int -- Uses an internal integer counter to generate fresh Ids

data CGState = CGState
    { idMap :: IdMap
    , instructions :: [Instruction]
    , idGen :: IdGenerator }

type CodeGen = State CGState

type LocalScope = HashMap VarIdentifier Id

initialCGState :: CGState
initialCGState = CGState (IdMap empty empty empty) [] (IdGenerator 0)

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

lookupVarType :: VarType -> CodeGen (Maybe Id)
lookupVarType typ = (lookup typ) . varTypes . idMap <$> get

lookupFuncType :: FuncType -> CodeGen (Maybe Id)
lookupFuncType typ = (lookup typ) . funcTypes . idMap <$> get

lookupFunction :: FuncIdentifier -> CodeGen (Maybe Id)
lookupFunction func = (lookup func) . functions . idMap <$> get


insertVarType :: VarType -> Id -> CodeGen ()
insertVarType typ typId = modify $ \st -> st { idMap = (idMap st) { varTypes = insert typ typId (varTypes (idMap st)) } }

insertFuncType :: FuncType -> Id -> CodeGen ()
insertFuncType typ typId = modify $ \st -> st { idMap = (idMap st) { funcTypes = insert typ typId (funcTypes (idMap st)) } }

insertFunction :: Id -> FuncIdentifier -> CodeGen ()
insertFunction funcId func = modify $ \st -> st { idMap = (idMap st) { functions = insert func funcId (functions (idMap st)) } }
