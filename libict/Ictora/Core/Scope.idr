module Ictora.Core.Scope

import Ictora.Core.Lang
import Ictora.Util

%access public export
%default total


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


insertIntoExprGlobals : (i : Fin (S n))
                     -> (newTy : CTy)
                     -> CExpr locals globals t
                     -> CExpr locals (insertAt i newTy globals) t
insertIntoExprGlobals i newTy (CLocalVar y) = CLocalVar y
insertIntoExprGlobals i newTy (CGlobalVar p) = CGlobalVar $ snd $ insertIntoIndexIs i newTy p
insertIntoExprGlobals i newTy (CLit x) = CLit x
insertIntoExprGlobals i newTy (CApp l r) = CApp (insertIntoExprGlobals i newTy l)
                                                (insertIntoExprGlobals i newTy r)
insertIntoExprGlobals i newTy (CLam x) = CLam (insertIntoExprGlobals i newTy x)
insertIntoExprGlobals i newTy (CLet x y) = CLet (insertIntoExprGlobals i newTy x)
                                                (insertIntoExprGlobals i newTy y)


insertIntoProgGlobals : (i : Fin (S n))
                     -> (newTy : CTy)
                     -> CProg globals
                     -> CProg (insertAt i newTy globals)
insertIntoProgGlobals n newTy CEmptyProg = CEmptyProg
insertIntoProgGlobals n newTy (CConsFunc f prog) =
    CConsFunc (insertIntoExprGlobals n newTy f) (insertIntoProgGlobals (FS n) newTy prog)


weakenGlobals : {t' : CTy}
             -> CExpr locals globals t
             -> CExpr locals (t' :: globals) t
weakenGlobals {t'} = insertIntoExprGlobals FZ t'
