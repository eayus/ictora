module Ictora.Core.LamLift

import Ictora.Core.Lang

data CExprNoLam : CExpr ty len scope -> Type where
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
        lamLiftExpr (CLam varName argTy body) = case lamLiftExpr body of
                                                     Just (auxTy ** auxName ** aux ** body') => Just (auxTy ** auxName ** aux ** CLam varName argTy (swapScope body'))
                                                     Nothing => let auxName = freshName scope in Nothing -- TODO


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
                                             Nothing => CConsFunc f $ lamLiftProg prog
