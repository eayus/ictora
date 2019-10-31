module CodeGen.Convert where

import Data.Maybe

import CodeGen.State
import SpirV.Lang
import SpirV.CodeGen
import SpirV.Operations
import SpirV.Options
import qualified SpirV.Type as ST
import Core.Type
import Core.Lang

addOp :: Operation NoResult -> CodeGen ()
addOp op = appendInstr $ Instruction op

addOpResult :: Operation Result -> CodeGen Id
addOpResult op = do
    res <- freshId
    appendInstr $ InstructionWithResult res op
    return res

getVarType :: CType -> CodeGen Id
getVarType typ = lookupVarType typ >>= maybe (cgVarType typ) return

getPtrType :: CType -> StorageClass -> CodeGen Id
getPtrType typ sc = lookupPtrType typ sc >>= maybe (cgPtrType typ sc) return

getFuncType :: FuncType -> CodeGen Id
getFuncType typ = lookupFuncType typ >>= maybe (cgFuncType typ) return

cgVarType :: CType -> CodeGen Id
cgVarType TFunc = error "Function types not supported yet"
cgVarType TInt = do
    tId <- addOpResult $ OpTypeInt 32 Signed
    insertVarType tId TInt
    return tId
cgVarType t@(TPair t1 t2) = do
    t1Id <- getVarType t1
    t2Id <- getVarType t2
    tId <- addOpResult $ OpTypeStruct [t1Id, t2Id]
    insertVarType tId t
    return tId

cgPtrType :: CType -> StorageClass -> CodeGen Id
cgPtrType typ sc = do
    nonPtrType <- getVarType typ
    ptrId <- addOpResult $ OpTypePointer sc nonPtrType
    insertPtrType ptrId typ sc
    return ptrId

cgFuncType :: FuncType -> CodeGen Id
cgFuncType t@(FuncType retType paramTypes) = do
    retTypeId <- getVarType retType
    paramTypeIds <- mapM getVarType paramTypes
    tId <- addOpResult $ OpTypeFunction retTypeId paramTypeIds
    insertFuncType tId t
    return tId


cgProgram :: Program -> CodeGen ()
cgProgram (Program funcs) = do
    addOp $ OpCapability ShaderCap
    addOp $ OpMemoryModel LogicalAddr SimpleMem

    inputId <- freshId
    vertexMainId <- freshId
    addOp $ OpEntryPoint VertexShader vertexMainId "vertex" [inputId]

    let inType = ST.TVec (ST.TInt 32 Unsigned)

    inputType <- getPtrType TInt InputStorage
    appendInstr $ InstructionWithResult inputId $ OpVariable inputType InputStorage Nothing


cgFunc :: FuncDef -> CodeGen ()
cgFunc (FuncDef funcType name params body) = do
    retTypeId <- getVarType $ returnType funcType
    funcTypeId <- getFuncType funcType

    funcId <- addOpResult $ OpFunction retTypeId (FunctionOptions MaybeInline Pure) funcTypeId
    paramIds <- cgFuncParams $ paramTypes funcType
    let scope = zip params paramIds

    bodyId <- cgExpr body scope
    addOp $ OpReturnValue bodyId
    addOp $ OpFunctionEnd

cgFuncParams :: [CType] -> CodeGen [Id]
cgFuncParams = mapM cgFuncParam

cgFuncParam :: CType -> CodeGen Id
cgFuncParam typ = getVarType typ >>= addOpResult . OpFunctionParameter
    

cgExpr :: Expr -> LocalScope -> CodeGen Id
cgExpr (Var name typ) scope = return . fromJust $ lookup name scope
cgExpr (Lit (LInt x)) scope = do
    typ <- getVarType TInt
    addOpResult $ OpConstant typ (IntLit x)
cgExpr (App funcId params typ) scope = do
    func <- fromJust <$> lookupFunction funcId
    paramIds <- mapM (flip cgExpr scope) params
    retTypeId <- getVarType typ
    addOpResult $ OpFunctionCall retTypeId func paramIds
cgExpr (MkPair e1 e2 typ) scope = do
    e1Id <- cgExpr e1 scope
    e2Id <- cgExpr e2 scope
    pairType <- getVarType typ
    pairPtrType <- getPtrType typ FunctionStorage
    pairPtr <- addOpResult $ OpVariable pairPtrType FunctionStorage Nothing
    let (TPair ltype rtype) = typ
    ltypePtr <- getPtrType ltype FunctionStorage
    rtypePtr <- getPtrType rtype FunctionStorage
    intType <- getVarType TInt
    const0 <- addOpResult $ OpConstant intType (IntLit 0)
    const1 <- addOpResult $ OpConstant intType (IntLit 1)
    ptrToFirst <- addOpResult $ OpAccessChain ltypePtr pairPtr [const0]
    ptrToSecond <- addOpResult $ OpAccessChain rtypePtr pairPtr [const1]
    addOp $ OpStore ptrToFirst e1Id NormalMemAccess
    addOp $ OpStore ptrToSecond e2Id NormalMemAccess
    addOpResult $ OpLoad pairType pairPtr NormalMemAccess
cgExpr (DePair (var1, var2) (ltype, rtype) pairExpr continuation _) scope = do
    pairExprId <- cgExpr pairExpr scope
    pairPtrType <- getPtrType (TPair ltype rtype) FunctionStorage
    pairVar <- addOpResult $ OpVariable pairPtrType FunctionStorage (Just pairExprId)
    ltypePtr <- getPtrType ltype FunctionStorage
    rtypePtr <- getPtrType rtype FunctionStorage
    intType <- getVarType TInt
    const0 <- addOpResult $ OpConstant intType (IntLit 0)
    const1 <- addOpResult $ OpConstant intType (IntLit 1)
    ptrToLeft <- addOpResult $ OpAccessChain ltypePtr pairVar [const0]
    ptrToRight <- addOpResult $ OpAccessChain rtypePtr pairVar [const1]
    ltypeId <- getVarType ltype
    rtypeId <- getVarType rtype
    left <- addOpResult $ OpLoad ltypeId ptrToLeft NormalMemAccess
    right <- addOpResult $ OpLoad rtypeId ptrToRight NormalMemAccess
    cgExpr continuation $ (var1, left) : (var2, right) : scope
