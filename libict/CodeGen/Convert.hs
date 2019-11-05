module CodeGen.Convert where

import Data.Maybe

import CodeGen.State
import SpirV.Lang
import SpirV.CodeGen
import SpirV.Operations
import SpirV.Options
import qualified SpirV.Type as SV
import qualified Core.Type as C
import Core.Lang

addOp :: Operation NoResult -> CodeGen ()
addOp op = appendInstr $ Instruction op

addOpResult :: Operation Result -> CodeGen Id
addOpResult op = do
    res <- freshId
    appendInstr $ InstructionWithResult res op
    return res

getVarType :: SV.VarType -> CodeGen Id
getVarType typ = lookupVarType typ >>= maybe (cgVarType typ) return

getFuncType :: SV.FuncType -> CodeGen Id
getFuncType typ = lookupFuncType typ >>= maybe (cgFuncType typ) return

cgVarType :: SV.VarType -> CodeGen Id
cgVarType typ@SV.TVoid = do
    typeId <- addOpResult $ OpTypeVoid
    insertVarType typ typeId
    return typeId
cgVarType typ@(SV.TInt width sign) = do
    typeId <- addOpResult $ OpTypeInt width sign
    insertVarType SV.TInt typ
    return typeId
cgVarType typ@SV.TBool = do
    typeId <- addOpResult $ OpTypeBool
    insertVarType typ typeId
    return typeId
cgVarType typ@(SV.TFloat width) = do
    typeId <- addOpResult $ OpTypeFloat width
    insertVarType typ typeId
    return typeId
cgVarType typ@(SV.TVec2 elemType) = do
    elemTypeId <- getVarType elemType
    typeId <- addOpResult $ OpTypeVector elemTypeId 2
    insertVarType typ typeId
    return typeId 
cgVarType typ@(SV.TVec3 elemType) = do
    elemTypeId <- getVarType elemType
    typeId <- addOpResult $ OpTypeVector elemTypeId 3
    insertVarType typ typeId
    return typeId
cgVarType typ@(SV.TStruct subTypes) = do
    subTypeIds <- mapM getVarType subTypes
    typeId <- addOpResult $ OpTypeStruct subTypeIds
    insertVarType typ typeId
    return typeId
cgVarType typ@(SV.TPtr derefType sc) = do
    derefTypeId <- getVarType derefType
    typeId <- addOpResult $ OpTypePtr sc derefTypeId
    insertVarType typ typeId
    return typeId


cgFuncType :: SV.FuncType -> CodeGen Id
cgFuncType typ@(SV.FuncType ret params) = do
    retId <- getVarType ret
    paramIds <- mapM getVarType params
    typeId <- addOpResult $ OpTypeFunction retId paramIds
    insertFuncType typ typeId
    return typeId


cgProgram :: Program -> CodeGen ()
cgProgram (Program funcs) = do
    addOp $ OpCapability ShaderCap
    addOp $ OpMemoryModel LogicalAddr SimpleMem

    inputId <- freshId
    vertexMainId <- freshId
    addOp $ OpEntryPoint VertexShader vertexMainId "vertex" [inputId]

    let inputType = SV.TPtr (SV.TVec3 $ SV.TInt 32 Unsigned) InputStorage

    inputTypeId <- getVarType inputType
    appendInstr $ InstructionWithResult inputId $ OpVariable inputTypeId InputStorage Nothing


cgFunc :: FuncDef -> CodeGen ()
cgFunc (FuncDef funcType name params body) = do
    let spirvFuncType = convert funcType
    retTypeId <- getVarType $ ret spirvFuncType
    funcTypeId <- getFuncType spirvFuncType

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
