module Ictora.Core.Lang

import public Data.Vect
import public Ictora.Core.Types

%access public export

CIdentifier : Type
CIdentifier = String

CScope : Nat -> Type
CScope n = Vect n (CIdentifier, CType)


data CExpr : CType -> (scopeLen : Nat) -> CScope scopeLen -> Type where
    CLam : (name : CIdentifier)
        -> (argT : CType)
        -> CExpr bodyT (S len) ((name, argT) :: scope)
        -> CExpr (CTArr argT bodyT) len scope

    CApp : CExpr (CTArr argT bodyT) len scope
        -> CExpr argT len scope
        -> CExpr bodyT len scope

    CVar : (name : CIdentifier)
        -> Elem (name, t) scope
        -> CExpr t len scope

    CLet : (name : CIdentifier)
        -> CExpr varT len scope
        -> CExpr exprT (S len) ((name, varT) :: scope)
        -> CExpr exprT len scope


record CFunction (name : CIdentifier) (ty : CType) (scopeLen : Nat) (scope : CScope scopeLen) where
    constructor MkCFunction
    body : CExpr ty scopeLen scope


data CProgram : (scopeLen : Nat) -> CScope scopeLen -> Type where
    CEmptyProg : CProgram len scope

    CConsFunc : CFunction name ty len scope
             -> CProgram (S len) ((name, ty) :: scope)





freeVariables : {ty : CType} -> CExpr ty len scope -> (scopeLen : Nat ** CScope scopeLen)
freeVariables (CLam name argTy body) = filter (\(s, _) => s != name) $ freeVariables body
freeVariables (CApp l r) = nub $ (freeVariables l) ++ (freeVariables r)
freeVariables (CVar name ty) = [(name, ty)]
freeVariables (CLet name var body) = freeVariables var

 
freshName : CScope len -> CIdentifier
freshName = tryNum 0
    where tryNum : Nat -> CScope len -> CIdentifier
          tryNum n scope = case lookup (show n) scope of
                                Just _ => tryNum (S n) scope
                                Nothing => (show n)

            -> CProgram len scope


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
