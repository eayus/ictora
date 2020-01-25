module Ictora.Core.Scope

import Ictora.Core.Lang

%access public export


weakenHasType : (newIndex : Fin (S n))
             -> (newTy : CTy)
             -> HasType i scope t
             -> (j : _ ** HasType j (insertAt newIndex newTy scope) t)
weakenHasType FZ newTy prf = (_ ** There prf)
weakenHasType (FS x) newTy Here = (FZ ** Here)
weakenHasType (FS x) newTy (There z) = let (j ** prf') = weakenHasType x newTy z in (FS j ** There prf')


weakenScope : (i : Fin (S n)) -> (ty : CTy) -> CExpr scope a -> CExpr (insertAt i ty scope) a
weakenScope n ty (CLit x) = CLit x
weakenScope n ty (CApp l r) = CApp (weakenScope n ty l) (weakenScope n ty r)
weakenScope n ty (CLam body) = CLam $ weakenScope (FS n) ty body
weakenScope n ty (CLet var body) = CLet (weakenScope n ty var) (weakenScope (FS n) ty body)
weakenScope n ty (CVar prf) = let (_ ** prf') = weakenHasType n ty prf in CVar prf'


scopeToType : Vect n CTy -> CTy -> CTy
scopeToType [] t' = t'
scopeToType (t :: ts) t' = scopeToType ts (t ~> t')


wrapLambdas : {scope : Vect n CTy} -> CExpr scope a -> CExpr [] (scopeToType scope a)
wrapLambdas {scope = []} e = e
wrapLambdas {scope = (x :: xs)} e = wrapLambdas $ CLam e


applyScope : (scope : Vect n CTy) -> CExpr [] (scopeToType scope a) -> CExpr scope a
applyScope [] e = e
applyScope (t :: ts) e = CApp (weakenScope FZ t $ applyScope ts e) (CVar Here)
