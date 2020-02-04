module Ictora.Core.Scope

import Ictora.Core.Lang
import Ictora.Util

%access public export


localsToType : Vect n CTy -> CTy -> CTy
localsToType [] t' = t'
localsToType (t :: ts) t' = localsToType ts $ t ~> t'


wrapLocals : {locals : Vect n CTy}
          -> CExpr locals globals t
          -> CExpr [] globals (localsToType locals t)
wrapLocals {locals = []} e = e
wrapLocals {locals = (t :: ts)} e = wrapLocals $ CLam e


applyLocals' : {locals : Vect n CTy}
            -> Suffix locals locals'
            -> CExpr locals' globals (localsToType locals t)
            -> CExpr locals' globals t
applyLocals' {locals = []} _ e = e
applyLocals' {locals = (t :: ts)} p e =
    CApp (applyLocals' (weakenSuffix p) e)
         (CLocalVar $ snd $ suffixIndexIs p This)


applyLocals : CExpr locals globals (localsToType locals t)
           -> CExpr locals globals t
applyLocals = applyLocals' SuffixRefl


weakenGlobals : CExpr locals globals t
             -> CExpr locals (t' :: globals) t
weakenGlobals (CLocalVar x) = CLocalVar x
weakenGlobals (CGlobalVar x) = CGlobalVar $ That x
weakenGlobals (CLit x) = CLit x
weakenGlobals (CApp l r) = CApp (weakenGlobals l) (weakenGlobals r)
weakenGlobals (CLam x) = CLam (weakenGlobals x)
weakenGlobals (CLet x y) = CLet (weakenGlobals x) (weakenGlobals y)
