module Ictora.Core.LamLift

import Ictora.Core.Lang
import Ictora.Core.Scope

%default total


liftExpr : {locals : Vect n CTy}
        -> {globals : Vect m CTy}
        -> {t : CTy}
        -> CExpr locals globals t
        -> let funcTy = localsToType locals t
        in (CExpr [] globals funcTy, CExpr locals (funcTy :: globals) t)
liftExpr e = (wrapLocals e, applyLocals $ CGlobalVar This)


lamLiftExpr : CExpr locals globals t
           -> Maybe (funcTy : CTy
                    ** (CExpr [] globals funcTy,
                       CExpr locals (funcTy :: globals) t))
lamLiftExpr (CLocalVar _) = Nothing
lamLiftExpr (CGlobalVar _) = Nothing
lamLiftExpr (CLit _) = Nothing
lamLiftExpr (CApp l r) =
    case lamLiftExpr l of
         Just (t ** (f, l')) => Just (t ** (f, CApp l' (weakenGlobals r)))
         Nothing => case lamLiftExpr r of
                         Just (t ** (f, r')) => Just (t ** (f, CApp (weakenGlobals l) r'))
                         Nothing => Nothing
lamLiftExpr (CLam body) =
    case lamLiftExpr body of
         Just (t ** (f, body')) => Just (t ** (f, CLam body'))
         Nothing => Just (_ ** liftExpr (CLam body))
lamLiftExpr (CLet x y) =
    case lamLiftExpr x of
         Just (t ** (f, x')) => Just (t ** (f, CLet x' (weakenGlobals y)))
         Nothing => case lamLiftExpr y of
                         Just (t ** (f, y')) => Just (t ** (f, CLet (weakenGlobals x) y'))
                         Nothing => Nothing


lamLiftFunc : CExpr locals globals t
           -> Maybe (funcTy : CTy
                    ** (CExpr [] globals funcTy,
                       CExpr locals (funcTy :: globals) t))
lamLiftFunc (CLam body) = case lamLiftFunc body of
                               Just (t ** (f, body')) => Just (t ** (f, CLam body'))
                               Nothing => Nothing
lamLiftFunc e = lamLiftExpr e
