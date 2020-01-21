module Ictora.Core.Scope

import Ictora.Core.Lang

%access public export


data SuperScope : CScope len1 -> CScope len2 -> Type where
    SSNil : SuperScope scope []
    SSCons : LookupIs x y scope
          -> SuperScope scope xys
          -> SuperScope scope ((x, y) :: xys)


weakenLookup : LookupIs x y scope -> SuperScope scope' scope -> LookupIs x y scope'
weakenLookup Here (SSCons lkp ss) = lkp
weakenLookup (There later neq) (SSCons lkp ss) = weakenLookup later ss


extendSuperScope : SuperScope bigScope smallScope
                -> NoKey varName smallScope
                -> SuperScope ((varName, varTy) :: bigScope) smallScope
extendSuperScope SSNil NoKeyEmpty = SSNil
extendSuperScope (SSCons z w) (NoKeyLater x f) = let s = extendSuperScope w x in SSCons (There z f) s


extendScopes : {smallScope : CScope _}
            -> {bigScope : CScope _}
            -> SuperScope bigScope smallScope
            -> SuperScope ((x, y) :: bigScope) ((x, y) :: smallScope)
extendScopes {smallScope = []} _ = SSCons Here SSNil
extendScopes {smallScope = ((varName, varTy) :: xs)} {bigScope} (SSCons lkp ss) = extendScopes (SSCons lkp ss)


superScopeRefl : (scope : CScope len) -> SuperScope scope scope


freshName : (scope : CScope len) -> (x : CIdentifier ** NoKey x scope)
freshName scope = firstUnique nameStream
    where
        nameStream : Stream CIdentifier
        nameStream = map (("lambda" ++) . show) $ iterate succ Z

        firstUnique : Stream CIdentifier -> (x : CIdentifier ** NoKey x scope)
        firstUnique (x :: xs) = case lookupOrMissing x scope of
                                     Left prf => (x ** prf)
                                     Right _ => assert_total $ firstUnique xs


typeUnderScope : CScope len -> CType -> CType
typeUnderScope [] t = t
typeUnderScope ((name, ty) :: xs) t = typeUnderScope xs (ty ~> t)


wrapLambdas : {scope : CScope len}
           -> CExpr ty scope
           -> CExpr (typeUnderScope scope ty) []
wrapLambdas {scope = []} e = e
wrapLambdas {scope = ((varName, varTy) :: xs)} e = wrapLambdas $ CLam varName varTy e


applyAll : {scope : CScope _}
        -> SuperScope superScope scope
        -> CExpr (typeUnderScope scope ty) superScope
        -> CExpr ty superScope
applyAll {scope = []} SSNil e = e
applyAll {scope = ((varName, varTy) :: xs)} (SSCons lkp ss) e = CApp (applyAll ss e) (CVar varName lkp)
