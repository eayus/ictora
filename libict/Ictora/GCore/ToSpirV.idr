module Ictora.GCore.ToSpirV

import Ictora.Util
import Ictora.GCore.Lang
import SpirV.Typed.Builder
import SpirV.Raw.Program
import Data.List
import Data.Vect
import Data.HVect


convertVarTy : GVarTy -> (t : TypeKind ** VarType t)
convertVarTy GTInt = (KScalar ** TInt 32 Unsigned)
convertVarTy GTBool = (KScalar ** TBool)
convertVarTy GTFloat = (KScalar ** TFloat 32)
convertVarTy (GTSum numFields subTypes) = (KComposite ** TStruct $ convertVarTyList subTypes)
    where convertVarTyList : Vect n GVarTy -> List (t : TypeKind ** VarType t)
          convertVarTyList [] = []
          convertVarTyList (x :: xs) = convertVarTy x :: convertVarTyList xs
convertVarTy (GTVec size) = (KComposite ** TVec (TFloat 32) (sizeToNat size))

convertFuncTy : GFuncTy -> FuncType
convertFuncTy (MkGFuncTy ret arity params) = let (_ ** retType) = convertVarTy ret in MkFuncType retType (map convertVarTy $ toList params)



sVarTy : GVarTy -> Builder Id
sVarTy t = let (_ ** t2) = convertVarTy t in getType t2


mutual
   {-- buildConstants : (tys : Vect n GVarTy) -> HVect (map InterpTy tys) -> Builder (List Id)
    buildConstants [] [] = pure []
    buildConstants (t :: ts) (x :: xs) = do
        xId <- buildConstant t x
        xsIds <- buildConstants ts xs
        pure (xId :: xsIds)--}


    buildConstant : (type : GVarTy) -> {auto p : GIsLiteral type} -> InterpTy type -> Builder Id
    buildConstant ty val = do
        res <- freshId
        stype <- sVarTy ty
        case ty of
             GTInt => addStaticInstr $ MkInstrWithRes res $ OpConstant stype (IntLit val)
             GTFloat => addStaticInstr $ MkInstrWithRes res $ OpConstant stype (FloatLit val)
             GTBool => addStaticInstr $ MkInstrWithRes res $ (if val then OpConstantTrue else OpConstantFalse) stype
             {--(GTVec size) => pure ()
             (GTSum len tys) => do
                 ids <- buildConstants tys val
                 addStaticInstr $ MkInstrWithRes res $ OpConstantComposite stype ids--}
                
        pure res


mutual
    buildExpr : {len : Nat} -> {scope : GScope len} -> {type : GVarTy} -> GExpr type len scope -> Vect len Id -> Builder (Id, List Instruction)
    buildExpr (GLit {type} val) _ = do
        i <- buildConstant type val
        pure (i, [])
    buildExpr {scope} (GVar name prf) scopeIds = do
        let nameIndex = elemIndex scope prf
        let nameId = index nameIndex scopeIds
        pure (nameId, [])
    buildExpr (GLet name var body) scopeIds = do
        (varId, varInstr) <- buildExpr var scopeIds
        (bodyId, bodyInstr) <- buildExpr body (varId :: scopeIds)
        pure (bodyId, varInstr ++ bodyInstr)
    buildExpr {type} {scope} (GFuncCall name prf paramList) scopeIds = do
        let nameIndex = elemIndex scope prf
        let nameId = index nameIndex scopeIds
        res <- freshId
        typeId <- sVarTy type

        (paramIds, paramInstrs) <- buildParams paramList scopeIds

        let instr = MkInstrWithRes res $ OpFunctionCall typeId nameId paramIds
        pure (res, paramInstrs ++ [instr])
    buildExpr {type = GTVec size} (GMkVec values) scopeIds = do
        (valueIds, valueCode) <- assert_total $ unzip <$> traverse (\val => buildExpr val scopeIds) (toList values)
        res <- freshId
        typeId <- sVarTy (GTVec size)
        pure (res, concat valueCode ++ [MkInstrWithRes res $ OpCompositeConstruct typeId valueIds])


    buildParams : GParamList len scope types -> Vect len Id -> Builder (List Id, List Instruction)
    buildParams EmptyParams _ = pure ([], [])
    buildParams (expr :: params) scopeIds = do
        (exprId, exprInstrs) <- buildExpr expr scopeIds
        (paramIds, paramInstrs) <- buildParams params scopeIds
        pure (exprId :: paramIds, exprInstrs ++ paramInstrs)



buildFunction : {type : GFuncTy} -> GFunction type len scope -> Vect len Id -> Builder Function
buildFunction {type} (MkGFunction name paramsNames body) scopeIds = do
    funcId <- freshId
    let opts = MkFunctionOptions Inline Pure
    paramIds <- traverse (const freshId) paramsNames

    (res, bodyCode) <- buildExpr body (paramIds ++ scopeIds)
    let ret = MkInstr $ OpReturnValue res
    labelId <- freshId
    let label = MkInstrWithRes labelId OpLabel

    pure $ MkFunction funcId (convertFuncTy type) (toList paramIds) opts ([label] ++ bodyCode ++ [ret])

buildFunctions : (prog : GProgram len scope) -> Vect len Id -> Builder (Vect (numFuncs prog) Function)
buildFunctions Nil _ = pure []
buildFunctions (Cons f prog) scopeIds = do
    sf <- buildFunction f scopeIds
    sfs <- buildFunctions prog (ident sf :: scopeIds)
    pure $ sf :: sfs


buildVertEntry : Vect n Function
              -> (funcIndex : Fin n)
              -> (glPerVertexVar : Id)
              -> Builder Function
buildVertEntry funcs funcIndex outVarId = do
    funcId <- freshId
    vertResult <- freshId
    let opts = MkFunctionOptions Inline Pure

    -- call "vert"
    -- set glPosition equal to "vert"
    -- return

    glPositionId <- freshId
    vec4Id <- sVarTy (GTVec Four)
    glPositionTypeId <- getType $ TPtr (TVec (TFloat 32) 4) OutputStorage
    let vertId = ident $ index funcIndex funcs
    const0 <- buildConstant GTInt 0
    labelId <- freshId

    -- Wondering why this isn't working? OpStore is backwards.
    let label = MkInstrWithRes labelId OpLabel
    let callVert = MkInstrWithRes vertResult $ OpFunctionCall vec4Id vertId []
    let getGlPosition = MkInstrWithRes glPositionId $ OpAccessChain glPositionTypeId outVarId [const0]
    let setGlPosition = MkInstr $ OpStore glPositionId vertResult NormalMemAccess
    let finish = MkInstr OpReturn

    pure $ MkFunction funcId (MkFuncType TVoid []) [] opts [label, callVert, getGlPosition, setGlPosition, finish]


buildFragEntry : (fragFunc : Id) -> (outColorVar : Id) -> Builder Function
buildFragEntry fragId color = do
    fragEntry <- freshId
    label <- freshId
    result <- freshId

    colorType <- sVarTy $ GTVec Four

    let iLabel = MkInstrWithRes label OpLabel
    let iCall  = MkInstrWithRes result $ OpFunctionCall colorType fragId []
    let iStore = MkInstr $ OpStore color result NormalMemAccess
    let iRet   = MkInstr OpReturn

    pure $ MkFunction fragEntry (MkFuncType TVoid []) [] (MkFunctionOptions Inline Pure) [iLabel, iCall, iStore, iRet]


buildModule : GCompleteProgram -> Builder Module
buildModule (MkGCompleteProgram prog vertProof fragProof) = do
    let caps = [ShaderCap, MatrixCap]

    sfuncs <- buildFunctions prog []

    let vertIndex = funcIndex prog vertProof
    let fragIndex = funcIndex prog fragProof

    -- Generate Input/Output Variables
    outType <- getType $ TPtr (TStruct [ convertVarTy (GTVec Four)
                                       , convertVarTy GTFloat
                                       , (KComposite ** TArray 1 (TFloat 32)) ]) OutputStorage
    outId <- freshId
    addStaticInstr $ MkInstrWithRes outId $ OpVariable outType OutputStorage Nothing

    colorVar <- freshId
    colorType <- getType $ TPtr (TVec (TFloat 32) 4) OutputStorage
    addStaticInstr $ MkInstrWithRes colorVar $ OpVariable colorType OutputStorage Nothing

    -- Build Entry Function
    vertEntryFunc <- buildVertEntry sfuncs vertIndex outId
    fragEntryFunc <- buildFragEntry (ident $ index fragIndex sfuncs) colorVar

    pure $ MkModule caps SimpleMem LogicalAddr (numFuncs prog) sfuncs vertEntryFunc [(outId, Nothing)] fragEntryFunc [(colorVar, Just 0)]


export convert : GCompleteProgram -> Program
convert = build . buildModule
