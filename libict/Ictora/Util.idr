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
