module Lang.Ict.DBIndexed

import public Lang.Ict.Common
import public Util.IndexIs
import Decidable.Equality


public export
data DExpr : (locals : Vect n IBasicTy) -> (tmpls : Vect m ITy) -> ITy -> Type where

    DApp : DExpr locals tmpls (IArrowTy a b)
        -> DExpr locals tmpls (INullaryTy a)
        -> DExpr locals tmpls b

    DTmplApp : DExpr locals tmpls (ITmplArrowTy a b)
        -> DExpr [] tmpls a
        -> DExpr locals tmpls b

    DLam : (a : IBasicTy)
        -> DExpr (a :: locals) tmpls b
        -> DExpr locals tmpls (IArrowTy a b)

    DTmplLam : (a : ITy)
        -> DExpr locals (a :: tmpls) b
        -> DExpr locals tmpls (ITmplArrowTy a b)

    DVar : {i : Fin _}
        -> IndexIs i locals a
        -> DExpr locals tmpls (INullaryTy a)

    DTmplVar : {i : Fin _}
        -> IndexIs i tmpls a
        -> DExpr locals tmpls a

    DLit : Int
        -> DExpr locals tmpls (INullaryTy IIntTy)



-- functions

padScope : DExpr locals tmpls a -> DExpr (locals ++ locals') (tmpls ++ tmpls') a
padScope (DApp l r) = DApp (padScope l) (padScope r)
padScope (DTmplApp l r) = DTmplApp (padScope l) (padScope r)
padScope (DLam t body) = DLam t $ padScope body
padScope (DTmplLam t body) = DTmplLam t $ padScope body
padScope (DVar prf) = DVar $ padIndexIs prf
padScope (DTmplVar prf) = DTmplVar $ padIndexIs prf
padScope (DLit x) = DLit x


public export
substituteTemplate : DExpr [] [] a
                  -> {j : Fin _}
                  -> IndexIs j tmpls a
                  -> DExpr locals tmpls b
                  -> DExpr locals (deleteAt j tmpls) b
substituteTemplate arg loc (DLit x) = DLit x
substituteTemplate arg loc (DApp l r) =
    DApp (substituteTemplate arg loc l) (substituteTemplate arg loc r)
substituteTemplate arg loc (DTmplApp l r) =
    DTmplApp (substituteTemplate arg loc l) (substituteTemplate arg loc r)
substituteTemplate arg loc (DLam t body) =
    DLam t $ substituteTemplate arg loc body
substituteTemplate arg loc (DTmplLam t body) =
    DTmplLam t $ substituteTemplate arg (There loc) body
substituteTemplate arg loc (DVar prf) = DVar prf
substituteTemplate arg {j} loc (DTmplVar {i} prf) = case decEq i j of
                                                        Yes Refl => let Refl = sameIndexSameElem loc prf
                                                                    in padScope arg
                                                        No contra => DTmplVar $ snd $ deleteAtIndexIs prf contra
