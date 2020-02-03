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


convertExpr : (locals : Scope)
           -> (globals : Scope)
           -> (uniq : UniqKeys globals)
           -> SExpr
           -> Either TypeError (t : _ ** IExpr locals (globals ** uniq) t)
convertExpr locals globals uniq (SVar name) =
  case lookupPrf name locals of
       Just (t ** prf) => Right (t ** ILocalVar prf)
       Nothing => case lookupPrf name globals of
                       Just (t ** prf) => Right (t ** IGlobalVar prf)
                       Nothing => Left $ UndefinedVariable name (fst <$> locals ++ globals)
convertExpr locals globals uniq (SApp x y) = do
    (lTy ** l) <- convertExpr locals globals uniq x
    (rTy ** r) <- convertExpr locals globals uniq y
    case lTy of
         (t1 ~> t2) => case decEq t1 rTy of
                            Yes Refl => pure (t2 ** IApp l r)
                            No _ => Left $ ArgTypeMismatch t1 rTy
         _ => Left $ NonFunctionApplication lTy rTy
convertExpr locals globals uniq (SLam var ty x) = do
    argTy <- convertTy ty
    (bodyTy ** body) <- convertExpr ((var, argTy) :: locals) globals uniq x
    pure (argTy ~> bodyTy ** ILam var argTy body)
convertExpr locals globals uniq (SLet var e1 e2) = do
    (e1Ty ** e1') <- convertExpr locals globals uniq e1
    (e2Ty ** e2') <- convertExpr ((var, e1Ty) :: locals) globals uniq e2
    pure (e2Ty ** ILet var e1' e2')
convertExpr locals globals uniq (SLit x) = pure (ITInt ** ILit x)


convertProg' : (globals : Scope) -> (uniq : UniqKeys globals) -> SProgram -> Either TypeError (IProg (globals ** uniq))
convertProg' globals uniq [] = pure Nil
convertProg' globals uniq ((name, expr) :: prog) =
    case decNoKey name globals of
         Just nokey => do
             (ty ** expr') <- convertExpr [] globals uniq expr
             prog' <- convertProg' ((name, ty) :: globals) (UniqCons nokey uniq) prog
             pure $ IConsFunc name nokey expr' prog'
         Nothing => Left $ DuplicateFunction name


convertProg : SProgram -> Either TypeError (IProg ([] ** UniqNil))
convertProg = convertProg' [] UniqNil
