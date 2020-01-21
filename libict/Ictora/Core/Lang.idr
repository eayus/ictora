module Ictora.Core.Lang

import public Ictora.Util
import public Data.Vect
import public Ictora.Core.Types
import public Data.Vect.Views

%access public export

CIdentifier : Type
CIdentifier = String

CScope : Nat -> Type
CScope n = Vect n (CIdentifier, CType)


data CExpr : CType -> CScope scopeLen -> Type where
    CLam : (name : CIdentifier)
        -> (argT : CType)
        -> CExpr bodyT ((name, argT) :: scope)
        -> CExpr (argT ~> bodyT) scope

    CApp : CExpr (argT ~> bodyT) scope
        -> CExpr argT scope
        -> CExpr bodyT scope

    CVar : (name : CIdentifier)
        -> LookupIs name t scope
        -> CExpr t scope

    CLet : (name : CIdentifier)
        -> CExpr varT scope
        -> CExpr exprT ((name, varT) :: scope)
        -> CExpr exprT scope


record CFunction (name : CIdentifier) (ty : CType) (scope : CScope scopeLen) where
    constructor MkCFunction
    body : CExpr ty scope


data CProgram : CScope scopeLen -> Type where
    CEmptyProg : CProgram scope

    CConsFunc : CFunction name ty scope
             -> CProgram ((name, ty) :: scope)
             -> CProgram scope


--LangConstruct : Type
--LangConstruct = (len : Nat) -> CScope len -> Type


{--decreaseMinScope : (m : Nat ** Vect m (varName : CIdentifier
                                      ** varTy : CType
                                      ** Elem (varName, varTy) (x :: scope)))
                -> (n : Nat ** Vect n (varName : CIdentifier
                                      ** varTy : CType
                                      ** Elem (varName, varTy) scope))
decreaseMinScope (Z ** []) = (Z ** [])
decreaseMinScope (S n ** ((varName ** varTy ** Here) :: xs)) = decreaseMinScope (n ** xs)
decreaseMinScope (S n ** ((varName ** varTy ** There p) :: xs)) =
    let (m ** ys) = decreaseMinScope (n ** xs)
    in  (S m ** (varName ** varTy ** p) :: ys)


minimumScope : {ty : CType}
            -> CExpr ty len scope
            -> (n : Nat ** Vect n (varName : CIdentifier
                                  ** varTy : CType 
                                  ** Elem (varName, varTy) scope))
minimumScope (CLam name ty body) = decreaseMinScope $ minimumScope body
minimumScope (CApp l r) = let (n1 ** scope1) = minimumScope l
                              (n2 ** scope2) = minimumScope r
                          in (n1 + n2 ** scope1 ++ scope2)
minimumScope {ty} (CVar name prf) = (1 ** [(name ** ty ** prf)])
minimumScope (CLet name var body) = decreaseMinScope $ minimumScope body


--TODO: IMPL THESE

minimumScope' : CExpr ty len scope -> (len' : Nat ** CScope len')

minimiseExprScope : (e : CExpr ty len scope)
                 -> CExpr ty (fst $ minimumScope' e)
                             (snd $ minimumScope' e)

weakenExprMany : CExpr ty len scope -> CExpr ty (len + extraLen) (scope ++ extraScope)

 
freshName : CScope len -> CIdentifier
freshName = tryNum 0
    where tryNum : Nat -> CScope len -> CIdentifier
          tryNum n scope = case lookup (show n) scope of
                                Just _ => tryNum (S n) scope
                                Nothing => (show n)



insertAtElemPrf : {xs : Vect n a} -> (i : Fin (S n)) -> Elem x xs -> Elem x (insertAt i y xs)
insertAtElemPrf FZ p = There p
insertAtElemPrf (FS n) Here = Here
insertAtElemPrf (FS n) (There p) = There $ insertAtElemPrf n p


weakenExpr : (i : Fin (S len))
          -> (x : (CIdentifier, CType))
          -> CExpr ty len scope
          -> CExpr ty (S len) (insertAt i x scope)
weakenExpr n x (CLam name ty body) = CLam name ty (weakenExpr (FS n) x body)
weakenExpr n x (CApp e1 e2) = CApp (weakenExpr n x e1) (weakenExpr n x e2)
weakenExpr n x (CVar name prf) = CVar name $ insertAtElemPrf n prf
weakenExpr n x (CLet name var body) = CLet name (weakenExpr n x var) (weakenExpr (FS n) x body)


weakenFunc : (i : Fin (S len))
          -> (x : (CIdentifier, CType))
          -> CFunction name ty len scope
          -> CFunction name ty (S len) (insertAt i x scope)
weakenFunc n x (MkCFunction body) = MkCFunction $ weakenExpr n x body


weakenProg : (i : Fin (S len))
          -> (x : (CIdentifier, CType))
          -> CProgram len scope
          -> CProgram (S len) (insertAt i x scope)
weakenProg n x CEmptyProg = CEmptyProg
weakenProg n x (CConsFunc f prog) = CConsFunc (weakenFunc n x f) (weakenProg (FS n) x prog)


swapPairAt : (i : Fin len) -> Vect (S len) a -> Vect (S len) a
swapPairAt FZ (x :: y :: xs) = (y :: x :: xs)
swapPairAt (FS n) (x :: xs) = x :: swapPairAt n xs


swapPairAtElemPrf : {xs : Vect (S len) a}
                 -> (i : Fin len)
                 -> Elem x xs
                 -> Elem x (swapPairAt i xs)
swapPairAtElemPrf {xs = x :: y :: xs} FZ Here = There Here
swapPairAtElemPrf {xs = x :: y :: xs} FZ (There Here) = Here
swapPairAtElemPrf {xs = x :: y :: xs} FZ (There (There prf)) = There $ There prf
swapPairAtElemPrf {xs = x :: xs} (FS n) Here = Here
swapPairAtElemPrf {xs = x :: xs} (FS n) (There prf) = There $ swapPairAtElemPrf n prf


swapScopeAt : (i : Fin len)
           -> CExpr ty (S len) scope
           -> CExpr ty (S len) (swapPairAt i scope)
swapScopeAt n (CLam name argTy body) = CLam name argTy $ swapScopeAt (FS n) body
swapScopeAt n (CApp l r) = CApp (swapScopeAt n l) (swapScopeAt n r)
swapScopeAt n (CVar name prf) = CVar name (swapPairAtElemPrf n prf)
swapScopeAt n (CLet name var body) = CLet name (swapScopeAt n var) (swapScopeAt (FS n) body)


swapScope : CExpr ty (S (S len)) (x :: y :: scope) -> CExpr ty (S (S len)) (y :: x :: scope)
swapScope = swapScopeAt FZ



scopeToType : CScope len -> CType -> CType
scopeToType [] ret = ret
scopeToType ((_, ty) :: xs) ret = CTArr ty (scopeToType xs ret)


applyAllScope : CExpr (scopeToType scope ty) len scope -> CExpr ty len scope--}
