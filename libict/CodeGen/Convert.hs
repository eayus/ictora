module CodeGen.Convert where

import Data.Maybe

import CodeGen.State
import SpirV.Lang
import Core.Type
import Core.Lang

-- TODO: SpirV does allow forward calls to functions, but need to know the ID in advance. Perhaps special system?

addOp :: Operation NoRet -> CodeGen ()
addOp op = appendInstr $ InstrNoRet op

addOpRet :: Operation Ret -> CodeGen Id
addOpRet op = do
    res <- freshId
    appendInstr $ InstrRet res op
    return res

getVarType :: CType -> CodeGen Id
getVarType typ = lookupVarType typ >>= maybe (cgVarType typ) return

getPtrType :: CType -> CodeGen Id
getPtrType typ = lookupPtrType typ >>= maybe (cgPtrType typ) return

getFuncType :: FuncType -> CodeGen Id
getFuncType typ = lookupFuncType typ >>= maybe (cgFuncType typ) return

cgVarType :: CType -> CodeGen Id
cgVarType TFunc = error "Function types not supported yet"
cgVarType TInt = do
    tId <- addOpRet $ OpTypeInt 32 Signed
    insertVarType tId TInt
    return tId
cgVarType t@(TPair t1 t2) = do
    t1Id <- getVarType t1
    t2Id <- getVarType t2
    tId <- addOpRet $ OpTypeStruct [t1Id, t2Id]
    insertVarType tId t
    return tId

cgPtrType :: CType -> CodeGen Id
cgPtrType typ = do
    nonPtrType <- getVarType typ
    ptrId <- addOpRet $ OpTypePointer FunctionStorage nonPtrType
    insertPtrType ptrId typ
    return ptrId

cgFuncType :: FuncType -> CodeGen Id
cgFuncType t@(FuncType retType paramTypes) = do
    retTypeId <- getVarType retType
    paramTypeIds <- mapM getVarType paramTypes
    tId <- addOpRet $ OpTypeFunction retTypeId paramTypeIds
    insertFuncType tId t
    return tId

cgFunc :: FuncDef -> CodeGen ()
cgFunc (FuncDef funcType name params body) = do
    retTypeId <- getVarType $ returnType funcType
    funcTypeId <- getFuncType funcType

    funcId <- addOpRet $ OpFunction retTypeId (FunctionControl MaybeInline Pure) funcTypeId
    paramIds <- cgFuncParams $ paramTypes funcType
    let scope = zip params paramIds

    bodyId <- cgExpr body scope
    addOp $ OpReturnValue bodyId
    addOp $ OpFunctionEnd

cgFuncParams :: [CType] -> CodeGen [Id]
cgFuncParams = mapM cgFuncParam

cgFuncParam :: CType -> CodeGen Id
cgFuncParam typ = getVarType typ >>= addOpRet . OpFunctionParameter
    

cgExpr :: Expr -> LocalScope -> CodeGen Id
cgExpr (Var name typ) scope = return . fromJust $ lookup name scope
cgExpr (Lit (LInt x)) scope = do
    typ <- getVarType TInt
    addOpRet $ OpConstant typ x
cgExpr (App funcId params typ) scope = do
    func <- fromJust <$> lookupFunction funcId
    paramIds <- mapM (flip cgExpr scope) params
    retTypeId <- getVarType typ
    addOpRet $ OpFunctionCall retTypeId func paramIds
cgExpr (MkPair e1 e2 typ) scope = do
    e1Id <- cgExpr e1 scope
    e2Id <- cgExpr e2 scope
    pairType <- getVarType typ
    pairPtrType <- getPtrType typ
    pairPtr <- addOpRet $ OpVariable pairPtrType FunctionStorage Nothing
    let (TPair ltype rtype) = typ
    ltypePtr <- getPtrType ltype
    rtypePtr <- getPtrType rtype
    intType <- getVarType TInt
    const0 <- addOpRet $ OpConstant intType 0
    const1 <- addOpRet $ OpConstant intType 1
    ptrToFirst <- addOpRet $ OpAccessChain ltypePtr pairPtr [const0]
    ptrToSecond <- addOpRet $ OpAccessChain rtypePtr pairPtr [const1]
    addOp $ OpStore ptrToFirst e1Id MemAccessNormal
    addOp $ OpStore ptrToSecond e2Id MemAccessNormal
    addOpRet $ OpLoad pairType pairPtr MemAccessNormal
cgExpr (DePair (var1, var2) (ltype, rtype) pairExpr continuation _) scope = do
    pairExprId <- cgExpr pairExpr scope
    pairPtrType <- getPtrType $ TPair ltype rtype
    pairVar <- addOpRet $ OpVariable pairPtrType FunctionStorage (Just pairExprId)
    ltypePtr <- getPtrType ltype
    rtypePtr <- getPtrType rtype
    intType <- getVarType TInt
    const0 <- addOpRet $ OpConstant intType 0
    const1 <- addOpRet $ OpConstant intType 1
    ptrToLeft <- addOpRet $ OpAccessChain ltypePtr pairVar [const0]
    ptrToRight <- addOpRet $ OpAccessChain rtypePtr pairVar [const1]
    ltypeId <- getVarType ltype
    rtypeId <- getVarType rtype
    left <- addOpRet $ OpLoad ltypeId ptrToLeft MemAccessNormal
    right <- addOpRet $ OpLoad rtypeId ptrToRight MemAccessNormal
    cgExpr continuation $ (var1, left) : (var2, right) : scope
