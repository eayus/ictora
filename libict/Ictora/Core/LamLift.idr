module Ictora.Core.LamLift

import Ictora.Core.Lang
import Ictora.Core.Scope


lambdaLiftExpr : {exprScope : CScope len}
              -> CExpr exprTy exprScope
              -> let funcTy = typeUnderScope exprScope exprTy
              in Maybe (funcName : CIdentifier   **   (CFunction funcName funcTy [], CExpr exprTy ((funcName, funcTy) :: exprScope)))
lambdaLiftExpr {exprScope} e@(CLam _ _ _) =
    let (freshName ** freshNamePrf) = freshName exprScope
        func = MkCFunction $ wrapLambdas e
        callFunc = CVar freshName Here
        e' = applyAll (extendSuperScope superScopeRefl freshNamePrf) callFunc
    in Just (freshName ** (func, e'))









































{--create : CFunction funcName funcTy funcScope
      -> CExpr exprTy ((funcName, funcTy) :: exprScope)
      -> (funcTy' : CType
         ** f : CFunction funcName funcTy' []
         ** CExpr exprTy ((funcName, funcTy') :: exprScope))


lambdaLiftExpr : CExpr exprTy myScope
              -> Maybe (funcName : CIdentifier
                       ** funcTy : CType
                       ** f : CFunction funcName funcTy []
                       ** CExpr exprTy ((funcName, funcTy) :: myScope))
lambdaLiftExpr expr@(CLam varName varType body) =
    let (auxTy ** aux ** expr') = create (MkCFunction expr) (CVar "testName" Here) in
        Just ("testName"
        ** auxTy
        ** aux
        ** expr')--}



{--reduceLiftedScope : (func : CFunction funcName funcTy (x :: funcScope))
                 -> (CExpr exprTy ((funcName, funcTy) :: exprScope))
                 -> 



liftExpr : CExpr exprTy exprScope
        -> (funcName : CIdentifier)
        -> (funcTy : CType
           ** func : CFunction funcName funcTy []
           ** CExpr exprTy ((funcName, funcTy) :: exprScope))
liftExpr e funcName = --}


{--liftExpr : CExpr ty scope
        -> (funcName : CIdentifier
           ** aux : CFunction funcName ty scope
           ** CExpr ty ((funcName, ty) :: scope))
liftExpr e = ("liftedName"
             ** MkCFunction e
             ** CVar "liftedName" Here)--}






































{--data CExprNoLam : CExpr ty len scope -> Type where
    CAppNoLam : CExprNoLam e1
             -> CExprNoLam e2
             -> CExprNoLam (CApp e1 e2)

    CVarNoLam : CExprNoLam (CVar name prf)

    CLetNoLam : CExprNoLam e1 
             -> CExprNoLam e2
             -> CExprNoLam (CLet name e1 e2)


data CExprNoInnerLam : CExpr ty len scope -> Type where
    CLamNoInnerLam : CExprNoInnerLam body
                  -> CExprNoInnerLam (CLam name ty body)

    CAppNoInnerLam : CExprNoLam e1
                  -> CExprNoLam e2
                  -> CExprNoInnerLam (CApp e1 e2)

    CVarNoInnerLam : CExprNoInnerLam (CVar name prf)

    CLetNoInnerLam : CExprNoLam e1
                  -> CExprNoLam e2
                  -> CExprNoInnerLam (CLet name e1 e2)


data CFuncNoInnerLam : CFunction name ty len scope -> Type where
    CMkFuncNoInnerLam : CExprNoInnerLam body
                     -> CFuncNoInnerLam (MkCFunction body)


data CProgNoInnerLam : CProgram len scope -> Type where
    CEmptyNoInnerLam : CProgNoInnerLam CEmptyProg

    CConsNoInnerLam : CFuncNoInnerLam func
                   -> CProgNoInnerLam prog
                   -> CProgNoInnerLam (CConsFunc func prog)


wrapLambda : (varName : CIdentifier) -> (varTy : CType) -> CExpr ty (S len) ((varName, varTy) :: scope) -> CExpr (CTArr varTy ty) len scope
--wrapLambda varName varTy e = CLam varName varTy e

wrapLambdas : {scope : CScope len} -> CExpr ty len scope -> CExpr (scopeToType scope ty) Z []
--wrapLambdas {scope = []} e = e
--wrapLambdas {scope = ((varName, varTy) :: xs)} e = wrapLambdas $ wrapLambda varName varTy e


lamLiftFunc : {scope : CScope len}
           -> CFunction name ty len scope
           -> Maybe (auxTy : CType
                ** auxName : CIdentifier
                ** aux : CFunction auxName auxTy len scope
                ** CFunction name ty (S len) ((auxName, auxTy) :: scope))
lamLiftFunc (MkCFunction funcBody) = ?lol
    where
        lamLiftExpr : CExpr exprTy exprLen exprScope
                     -> Maybe (auxTy : CType
                              ** auxName : CIdentifier
                              ** aux : CFunction auxName auxTy len scope
                              ** CExpr exprTy (S exprLen) ((auxName, auxTy) :: exprScope))
        lamLiftExpr e@(CLam varName argTy body) = case lamLiftExpr body of
                                                     Just (auxTy ** auxName ** aux ** body') => Just (auxTy ** auxName ** aux ** CLam varName argTy (swapScope body'))
                                                     Nothing => let (_ ** minScope) = minimumScope' e
                                                                    auxName = freshName scope
                                                                    auxBody = wrapLambdas $ minimiseExprScope e
                                                                    --e' = buildNewExpr
                                                                    e' = applyAllScope (CVar auxName Here)
                                                                in Nothing
                                                  where
                                                      f : Int
                                                      f = 5


        lamLiftInners : CExpr exprTy exprLen exprScope
                     -> Maybe (auxTy : CType
                              ** auxName : CIdentifier
                              ** aux : CFunction auxName auxTy len scope
                              ** CExpr exprTy (S exprLen) ((auxName, auxTy) :: exprScope))
        lamLiftInners (CLam varName argTy body) = case lamLiftInners body of
                                                    Just (auxTy ** auxName ** aux ** body') => Just $ (auxTy ** auxName ** aux ** CLam varName argTy (swapScope body'))
                                                    Nothing => Nothing
        lamLiftInners (CApp l r) = case lamLiftExpr l of
                                        Just (auxTy ** auxName ** aux ** l') => Just $ (auxTy ** auxName ** aux ** CApp l' (weakenExpr FZ (auxName, auxTy) r))
                                        Nothing => case lamLiftExpr r of
                                                        Just (auxTy ** auxName ** aux ** r') => Just $ (auxTy ** auxName ** aux ** CApp (weakenExpr FZ (auxName, auxTy) l) r')
                                                        Nothing => Nothing
        lamLiftInners (CVar _ _) = Nothing
        lamLiftInners (CLet varName var body) = case lamLiftExpr var of
                                                  Just (auxTy ** auxName ** aux ** var') => Just $ (auxTy ** auxName ** aux ** CLet varName var' (weakenExpr (FS FZ) (auxName, auxTy) body))
                                                  Nothing => case lamLiftExpr body of
                                                                  Just (auxTy ** auxName ** aux ** body') => Just (auxTy ** auxName ** aux ** CLet varName (weakenExpr FZ (auxName, auxTy) var) (swapScope body'))


lamLiftProg : CProgram len scope -> CProgram len scope
lamLiftProg CEmptyProg = CEmptyProg
lamLiftProg (CConsFunc f prog) = case lamLiftFunc f of
                                             Just (auxTy ** auxName ** auxFunc ** f') => assert_total $ lamLiftProg $ CConsFunc auxFunc $ CConsFunc f' $ weakenProg (FS FZ) (auxName, auxTy) prog
                                             Nothing => CConsFunc f $ lamLiftProg prog--}
