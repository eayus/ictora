module Ictora.Util

import Data.Fin
import Data.List
import Data.Vect
import Data.HVect

%access public export


Assoc : Type -> Type -> Type
Assoc x y = List (x, y)


listToVect : (xs : List a) -> Vect (length xs) a
listToVect [] = []
listToVect (x :: xs) = x :: listToVect xs


elemIndex : (xs : Vect n a) -> Elem x xs -> Fin n
elemIndex (x :: xs) Here = FZ
elemIndex (x :: xs) (There later) = FS $ elemIndex xs later

append : Vect n a -> a -> Vect (S n) a
append Nil x = x :: Nil
append (y :: ys) x = y :: append ys x

