module Util.IndexIs

import public Data.Fin
import public Data.Vect

%default total


public export
data IndexIs : Fin len -> Vect len elem -> elem -> Type where
   Here : IndexIs FZ (e :: xs) e
   There : IndexIs n xs e -> IndexIs (FS n) (x :: xs) e


public export
sameIndexSameElem : IndexIs i xs e1 -> IndexIs i xs e2 -> (e1 = e2)
sameIndexSameElem Here Here = Refl
sameIndexSameElem (There x) (There y) = sameIndexSameElem x y


public export
padIndexIs : IndexIs i xs e -> IndexIs (weakenN _ i) (xs ++ ys) e
padIndexIs Here = Here
padIndexIs (There p) = There $ padIndexIs p


public export
deleteAtIndexIs : {len : Nat}
               -> {i : Fin (S len)}
               -> {j : Fin (S len)}
               -> {xs : Vect (S len) a}
               -> IndexIs i xs e
               -> Not (i = j)
               -> (k ** IndexIs k (deleteAt j xs) e)
deleteAtIndexIs {len} {i = FZ} {j = FZ} {xs = x :: xs} _ neq = void $ neq Refl
deleteAtIndexIs {len = Z} {i = (FS n)} {j} {xs = x :: xs} _ neq = absurd n
deleteAtIndexIs {len = S n} {i = FZ} {j = (FS k)} {xs = x :: xs} Here neq = (_ ** Here)
deleteAtIndexIs {len = S n} {i = (FS k)} {j = FZ} {xs = x :: xs} (There p) neq = (k ** p)
deleteAtIndexIs {len = S n} {i = (FS k)} {j = (FS l)} {xs = x :: xs} (There prf) neq =
    case decEq k l of
        Yes Refl => void $ neq Refl
        No contra => let (u ** v) = deleteAtIndexIs prf contra
                     in (FS u ** There v)
