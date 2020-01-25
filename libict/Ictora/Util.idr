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


data LookupIs : (key : a) -> Vect n (a, b) -> (val : b) -> Type where
    Here : LookupIs x ((x, y) :: xys) y
    There : LookupIs x xys y -> Not (x = newX) -> LookupIs x ((newX, newY) :: xys) y


data NoKey : (key : a) -> Vect n (a, b) -> Type where
    NoKeyEmpty : NoKey k []
    NoKeyLater : NoKey k xys -> Not (x = k) -> NoKey k ((x, y) :: xys)


data UniqueKeys : Vect n (a, b) -> Type where
    UniqEmpty : UniqueKeys []
    UniqCons : UniqueKeys xs -> NoKey k xs -> UniqueKeys ((k, v) :: xs)


lookupOrMissing : DecEq a => (key : a) -> (m : Vect n (a, b)) -> Either (NoKey key m) b
lookupOrMissing k [] = Left $ NoKeyEmpty
lookupOrMissing k ((x, y) :: xys) = case decEq x k of
                                         Yes _ => Right y
                                         No neqPrf => case lookupOrMissing k xys of
                                                        Left noKeyPrf => Left $ NoKeyLater noKeyPrf neqPrf
                                                        Right val => Right val


Some : {a : Type} -> (a -> Type) -> Type
Some {a} f = (x : a ** f x)



removeEntriesWithKey : DecEq a
                    => (key : a)
                    -> Vect len (a, b)
                    -> (len' : Nat
                       ** map' : Vect len' (a, b)
                       ** NoKey key map')
removeEntriesWithKey key [] = (0 ** [] ** NoKeyEmpty)
removeEntriesWithKey key ((k, v) :: xs) = case decEq k key of
                                               Yes _ => removeEntriesWithKey key xs
                                               No prf => let (len' ** map' ** nokey') = removeEntriesWithKey key xs in (S len' ** (k, v) :: map' ** NoKeyLater nokey' prf)
