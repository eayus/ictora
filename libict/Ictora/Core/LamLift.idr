module Ictora.Core.LamLift

import Ictora.Core.Lang
import Ictora.Core.Scope


data ExprNoLam : CExpr a scope -> Type where
    LitNoLam : ExprNoLam (CLit _)
    AppNoLam : ExprNoLam l -> ExprNoLam r -> ExprNoLam (CApp l r)
    VarNoLam : ExprNoLam (CVar _)
    LetNoLam : ExprNoLam var -> ExprNoLam body -> ExprNoLam (CLet var body)


data ExprNoInnerLam : CExpr a scope -> Type where
    LitNoInnerLam : ExprNoInnerLam (CLit _)
    AppNoInnerLam : ExprNoLam l -> ExprNoLam r -> ExprNoInnerLam (CApp l r)
    LamNoInnerLam : ExprNoInnerLam body -> ExprNoInnerLam (CLam body)
    VarNoInnerLam : ExprNoInnerLam (CVar _)
    LetNoInnerLam : ExprNoLam var -> ExprNoInnerLam body -> ExprNoInnerLam (CLet var body)


liftExpr : CExpr scope a
       -> let auxTy = scopeToType scope a
       in (CExpr [] auxTy,
           CExpr (auxTy :: scope) a)
liftExpr e = (wrapLambdas e, applyScope (SuffixCons SuffixRefl) $ CVar Here)


{--lamLiftExpr : (e : CExpr scope a)
           -> Either
                  (auxTy : CTy ** (CExpr [] auxTy, CExpr (auxTy :: scope) a))
                  (ExprNoLam e)
lamLiftExpr (CLit _) = Right LitNoLam
lamLiftExpr (CVar _) = Right VarNoLam
lamLiftExpr (CApp l r) = ?help1
lamLiftExpr (CLet x y) = ?help_5
lamLiftExpr e@(CLam _) = Left $ (_ ** liftExpr e)--}
