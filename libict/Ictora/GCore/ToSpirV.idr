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


sVarTy : GVarTy -> Builder Id
sVarTy t = let (_ ** t2) = convertVarTy t in getType t2


mutual
    buildConstants : (tys : Vect n GVarTy) -> HVect (map InterpTy tys) -> Builder (List Id)
    buildConstants [] [] = pure []
    buildConstants (t :: ts) (x :: xs) = do
        xId <- buildConstant t x
        xsIds <- buildConstants ts xs
        pure (xId :: xsIds)


    buildConstant : (type : GVarTy) -> InterpTy type -> Builder Id
    buildConstant ty val = do
        res <- freshId
        stype <- sVarTy ty
        case ty of
             GTInt => addStaticInstr $ MkInstrWithRes res $ OpConstant stype (IntLit val)
             _ => pure ()
             GTFloat => addStaticInstr $ MkInstrWithRes res $ OpConstant stype (FloatLit val)
             GTBool => addStaticInstr $ MkInstrWithRes res $ (if val then OpConstantTrue else OpConstantFalse) stype
             (GTSum len tys) => do
                 ids <- buildConstants tys val
                 addStaticInstr $ MkInstrWithRes res $ OpConstantComposite stype ids
                
        pure res


buildExpr : GExpr type scope -> Builder (Id, List Instruction)
buildExpr (GLit {type} val) = do
    i <- buildConstant type val
    pure (i, [])
buildExpr (GVar name prf) = do
    res <- freshId
    pure (res, [])
buildExpr _ = do
    res <- freshId
    pure (res, [])
{--buildExpr (GLet var expr body) = do
    res <- freshId

    exprId <- buildExpr expr

    

    pure (res, [])--}


buildFunction : GFunction _ _ -> Builder Function
{--buildFunction (MkGFunction name params body) = do
    funcId <- freshId
    let opts = MkFunctionOptions Inline Pure
    paramIds <- traverse (const freshId) params--}


buildFunctions : (prog : GProgram scope) -> Builder (Vect (numFuncs prog) Function)
buildFunctions Nil = pure []
buildFunctions (Cons f prog) = do
    sf <- buildFunction f
    sfs <- buildFunctions prog
    pure $ sf :: sfs


buildModule : GCompleteProgram -> Builder Module
buildModule (MkGCompleteProgram prog vertProof fragProof) = do
    let caps = [ShaderCap, MatrixCap]

    sfuncs <- buildFunctions prog

    let vertIndex = funcIndex prog vertProof
    let fragIndex = funcIndex prog fragProof

    pure $ MkModule caps SimpleMem LogicalAddr (numFuncs prog) sfuncs vertIndex fragIndex


{--
convert : GCompleteProgram -> Program
convert = build . buildModule--}
