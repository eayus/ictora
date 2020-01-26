module Ictora.Core.Scope

import Ictora.Core.Lang

%access public export


data Suffix : Vect n a -> Vect m a -> Type where
    SuffixRefl : Suffix xs xs
    SuffixCons : Suffix xs ys -> Suffix xs (y :: ys)


weakenSuffix : Suffix (x :: xs) ys -> Suffix xs ys
weakenSuffix SuffixRefl = SuffixCons SuffixRefl
weakenSuffix (SuffixCons x) = SuffixCons $ weakenSuffix x


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


suffixHasType : Suffix scope scope' -> HasType i scope t -> (j : _ ** HasType j scope' t)
suffixHasType SuffixRefl hasTy = (_ ** hasTy)
suffixHasType (SuffixCons x) p = let (suff' ** hasTy') = suffixHasType x p in (FS suff' ** There hasTy')


applyScope : {scope : Vect n CTy}
          -> Suffix scope scope'
          -> CExpr scope' (scopeToType scope a)
          -> CExpr scope' a
applyScope {scope = []} _ e = e
applyScope {scope = (t :: ts)} p e = CApp (applyScope (weakenSuffix p) e) (let (_ ** hastype) = suffixHasType p Here in CVar hastype)
