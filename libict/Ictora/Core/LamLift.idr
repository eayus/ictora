module Ictora.Core.LamLift

import Ictora.Core.Lang
import Ictora.Core.Scope

%default total


liftExpr : CExpr locals globals t
        -> let funcTy = localsToType locals t
        in (CExpr [] globals funcTy, CExpr locals (funcTy :: globals) t)
liftExpr e = (wrapLocals e, applyLocals $ CGlobalVar This)


liftExprPreservesNIL : NoInnerLam e -> NoInnerLam (fst (liftExpr e))
liftExprPreservesNIL p = wrapLocalsPreservesNIL p


lamLiftExpr : (e : CExpr locals globals t)
           -> Either
                  (NoLam e)
                  (funcTy : CTy ** (
                      (aux : CExpr [] globals funcTy ** NoInnerLam aux),
                      CExpr locals (funcTy :: globals) t
                  ))
lamLiftExpr (CLocalVar x) = Left NLLocalVar
lamLiftExpr (CGlobalVar x) = Left NLGlobalVar
lamLiftExpr (CLit x) = Left NLLit
lamLiftExpr (CApp l r) =
    case lamLiftExpr l of
         Right (t ** (aux, l')) => Right (t ** (aux, CApp l' (weakenGlobals r)))
         Left nll => case lamLiftExpr r of
                         Right (t ** (aux, r')) => Right (t ** (aux, CApp (weakenGlobals l) r'))
                         Left nlr => Left $ NLApp nll nlr
lamLiftExpr (CLam body) =
    case lamLiftExpr body of
         Right (t ** (aux, body')) => Right (t ** (aux, CLam body'))
         Left nlbody => let x = liftExpr (CLam body)
                        in Right (_ ** ((fst x ** liftExprPreservesNIL $ OuterLam $ NoneAtAll nlbody), snd x))
lamLiftExpr (CLet x y) =
    case lamLiftExpr x of
         Right (t ** (aux, x')) => Right (t ** (aux, CLet x' (weakenGlobals y)))
         Left nlx => case lamLiftExpr y of
                         Right (t ** (aux, y')) => Right (t ** (aux, CLet (weakenGlobals x) y'))
                         Left nly => Left $ NLLet nlx nly

lamLiftFunc : (e : CExpr locals globals t)
           -> Either
                  (NoInnerLam e)
                  (funcTy : CTy ** (
                      (aux : CExpr [] globals funcTy ** NoInnerLam aux),
                      CExpr locals (funcTy :: globals) t
                  ))
lamLiftFunc (CLam body) = case lamLiftFunc body of
                               Right (t ** (f, body')) => Right (t ** (f, CLam body'))
                               Left bodynil => Left $ OuterLam bodynil
lamLiftFunc e = case lamLiftExpr e of
                     Left nle => Left $ NoneAtAll nle
                     Right x => Right x


lamLiftProg : CProg globals -> (p : CProg globals ** LamLifted p)
lamLiftProg CEmptyProg = (CEmptyProg ** LLEmpty)
lamLiftProg (CConsFunc f prog) =
    case lamLiftFunc f of
         Right (t ** ((aux ** nilaux), f')) =>
             let (p' ** llp') = assert_total $ lamLiftProg $ CConsFunc f' $ insertIntoProgGlobals (FS FZ) t prog
             in (CConsFunc aux p' ** LLConsFunc nilaux llp')
         Left fnil => let (p' ** llp') = lamLiftProg prog
                      in (CConsFunc f p' ** LLConsFunc fnil llp')
