module CodeGen where

import Control.Lens
import Control.Monad.State.Lazy

import Core.Lang
import Core.Type
import SpirV.Lang

data TypeMap = TypeMap
    { _normalTypes :: [(CType, Id)]
    , _functionTypes :: [(FuncType, Id)] }

data CodeGenState = CodeGenState
    { _typeMap :: TypeMap
    , _instructions :: [Instruction]
    , _nextIdNum :: Int }

makeLenses ''TypeMap
makeLenses ''CodeGenState

-- perhaps we add instructions in reverse order for more performance, rather than appending to list all the time!

initialState :: CodeGenState
initialState = CodeGenState (TypeMap [] []) [] 0

freshId :: State CodeGenState Id
freshId = Id . show <$> (nextIdNum <%= (+1))
{--freshId = do
    (CodeGenState types inst nextId) <- get
    put $ CodeGenState types inst (nextId + 1)
    return $ Id $ show nextId--}
    

appendInstr :: Instruction -> State CodeGenState ()
appendInstr newInstr = do
    (CodeGenState types inst nextId) <- get
    put $ CodeGenState types (inst ++ [newInstr]) nextId
    
addOp :: Operation NoRet -> State CodeGenState ()
addOp op = appendInstr $ InstrNoRet op

addOpRet :: Operation Ret -> State CodeGenState Id
addOpRet op = do
    res <- freshId
    appendInstr $ InstrRet res op
    return res

getType :: CType -> State CodeGenState Id
getType typ = do
    types <- _normalTypes . _typeMap <$> get
    case lookup typ types of
        Just typeId -> return typeId
        Nothing -> codegenType typ

-- TODO: codegenType should add to the typemap
codegenType :: CType -> State CodeGenState Id
codegenType TFunc = error "Passing functions to functions not supported yet (coming soon)"
codegenType TInt = addOpRet $ OpTypeInt 32 Signed
codegenType (TPair t1 t2) = do
    t1Id <- getType t1
    t2Id <- getType t2
    addOpRet $ OpTypeStruct [t1Id, t2Id]


codegenFunc :: FuncDef -> State CodeGenState ()
codegenFunc (FuncDef (FuncType retType paramTypes) name body) = do
    retTypeId <- getType retType
    return ()
