module Ictora.Syntax.ToIct

import Ictora.Syntax.Lang
import Ictora.Syntax.TypeError
import Ictora.Ict.Lang
import Ictora.Misc
import Control.Monad.Reader
import Control.Monad.Trans

%default total


Scope : Type
Scope = Map String ITy


convertTy : STy -> Either TypeError ITy
convertTy (NamedTy "Int") = pure ITInt
convertTy (NamedTy "Float") = pure ITFloat
convertTy (NamedTy badName) = Left $ UnknownType badName []
convertTy (t ~> u) = [| convertTy t ~> convertTy u |]


convertExpr : (scope : Scope) -> SExpr -> Either TypeError (t : _ ** IExpr scope t)
convertExpr scope (SVar name) = case lookupPrf name scope of
                                     Just (t ** prf) => Right (t ** IVar prf)
                                     Nothing => Left $ UndefinedVariable name (fst <$> scope)
convertExpr scope (SApp x y) = do
    (lTy ** l) <- convertExpr scope x
    (rTy ** r) <- convertExpr scope y
    case lTy of
         (t1 ~> t2) => case decEq t1 rTy of
                            Yes Refl => pure (t2 ** IApp l r)
                            No _ => Left $ ArgTypeMismatch t1 rTy
         _ => Left $ NonFunctionApplication lTy rTy
convertExpr scope (SLam var ty x) = do
    argTy <- convertTy ty
    (bodyTy ** body) <- convertExpr ((var, argTy) :: scope) x
    pure (argTy ~> bodyTy ** ILam var argTy body)
convertExpr scope (SLet var e1 e2) = do
    (e1Ty ** e1') <- convertExpr scope e1
    (e2Ty ** e2') <- convertExpr ((var, e1Ty) :: scope) e2
    pure (e2Ty ** ILet var e1' e2')
convertExpr scope (SLit x) = pure (ITInt ** ILit x)


convertProg' : (scope : Scope) -> (uniq : UniqKeys scope) -> SProgram -> Either TypeError (IProg (scope ** uniq))
convertProg' scope uniq [] = pure Nil
convertProg' scope uniq ((name, expr) :: prog) =
    case decNoKey name scope of
         Just nokey => do
             (ty ** expr') <- convertExpr scope expr
             prog' <- convertProg' ((name, ty) :: scope) (UniqCons nokey uniq) prog
             pure $ IConsFunc name nokey expr' prog'
         Nothing => Left $ DuplicateFunction name


convertProg : SProgram -> Either TypeError (IProg ([] ** UniqNil))
convertProg = convertProg' [] UniqNil
