module Ictora.Syntax.ToIct

import Ictora.Syntax.Lang
import Ictora.Syntax.TypeError
import Ictora.Ict.Lang
import Control.Monad.Reader
import Control.Monad.Trans

%default total


TypeAliases : Type
TypeAliases = List (SIdent, ITy)

Scope : Nat -> Type
Scope n = Vect n (SIdent, ITy)



defaultTAs : TypeAliases
defaultTAs = [ ("Int", ITInt), ("Float", ITFloat) ]


convertTy : TypeAliases -> STy -> Either TypeError ITy
convertTy tas (t ~> u) = [| convertTy tas t ~> convertTy tas u |]
convertTy tas (NamedTy name) = case lookup name tas of
                                    Just ity => pure ity
                                    Nothing => Left $ UnknownType name (fst <$> tas)


convertExpr : TypeAliases
           -> (locals : Scope n)
           -> (globals : Scope m)
           -> SExpr
           -> Either TypeError (ty : ITy ** IExpr (snds locals) globals ty)
convertExpr tas locals globals (SVar name) = case lookupPrf name locals of
                                                  Just (ty ** prf) => let (i ** loc ) = keyLocation prf in Right $ (ty ** ILocalVar loc)
                                                  Nothing => case lookupPrf name globals of
                                                                  Just (ty ** prf) => Right (ty ** IGlobalVar prf)
                                                                  Nothing => Left $ UndefinedVariable name (toList . map fst $ locals ++ globals)
convertExpr tas locals globals (SApp l r) = do
    (lTy ** l') <- convertExpr tas locals globals l
    (rTy ** r') <- convertExpr tas locals globals r
    case lTy of
         (t1 ~> t2) => case decEq t1 rTy of
                            Yes Refl => Right $ (t2 ** IApp l' r')
                            No _ => Left $ ArgTypeMismatch t1 rTy

         _ => Left $ NonFunctionApplication lTy rTy
convertExpr tas locals globals (SLam var ty x) = do
    argTy <- convertTy tas ty
    (bodyTy ** body) <- convertExpr tas ((var, argTy) :: locals) globals x
    Right (argTy ~> bodyTy ** ILam body)
convertExpr tas locals globals (SLet name e1 e2) = do
    (e1Ty ** e1') <- convertExpr tas locals globals e1
    (e2Ty ** e2') <- convertExpr tas ((name, e1Ty) :: locals) globals e2
    Right (e2Ty ** ILet e1' e2')
convertExpr tas locals globals (SLit x) = Right (ITInt ** ILit x)


convertProg : TypeAliases 
           -> (globals : Scope m)
           -> SProgram
           -> Either TypeError (IProgram globals)
convertProg _ _ [] = Right IEmptyProg
convertProg tas globals ((name, e) :: xs) = do
    (bodyT ** body) <- convertExpr tas [] globals e
    xs' <- convertProg tas ((name, bodyT) :: globals) xs
    Right $ IConsFunc name body xs'


export convert : SProgram -> Either TypeError (IProgram [])
convert = convertProg defaultTAs []
