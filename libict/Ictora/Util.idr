module Ictora.Util

import Data.Fin
import Data.List
import Data.Vect
import Data.HVect

%access public export
%default total


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


data NotElem : (x : a) -> (xs : Vect n a) -> Type where
    NotElemNil : NotElem x []
    NotElemCons : Not (x = y) -> NotElem x ys -> NotElem x (y :: ys)


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






-- Actually Useful Stuff

data IndexIs : Fin n -> Vect n a -> a -> Type where
    This : IndexIs FZ (e :: xs) e
    That : IndexIs n xs e -> IndexIs (FS n) (x :: xs) e


lookupPrf : DecEq a => (k : a) -> (xs : Vect n (a, b)) -> Maybe (v : b ** LookupIs k xs v)
lookupPrf k [] = Nothing
lookupPrf k ((k', v') :: xs) = case decEq k k' of
                                    Yes Refl => Just (v' ** Here)
                                    No prf => do
                                        (v'' ** later) <- lookupPrf k xs
                                        Just (v'' ** There later prf)



snds : Vect n (a, b) -> Vect n b
snds [] = []
snds ((x, y) :: xys) = y :: snds xys


lookupIndexPrf : DecEq a => {n : Nat} -> (k : a) -> (xs : Vect n (a, b)) -> Maybe (val : b ** i : Fin n ** IndexIs i (snds xs) val)
lookupIndexPrf {n = Z} k [] = Nothing
lookupIndexPrf {n = S n'} k ((k', v') :: xys) = case decEq k k' of
                                          Yes Refl => Just (v' ** _ ** This)
                                          No _ => do
                                              (val ** i ** prf) <- lookupIndexPrf k xys
                                              Just (val ** FS i ** That prf)


keyLocation : LookupIs k xs v -> (i : _ ** IndexIs i (snds xs) v)
keyLocation Here = (FZ ** This)
keyLocation (There later prf) = let (i ** p) = keyLocation later in (FS i ** That p)


data Suffix : Vect n a -> Vect m a -> Type where
    SuffixRefl : Suffix xs xs
    SuffixCons : Suffix xs ys -> Suffix xs (y :: ys)


weakenSuffix : Suffix (x :: xs) ys -> Suffix xs ys
weakenSuffix SuffixRefl = SuffixCons SuffixRefl
weakenSuffix (SuffixCons x) = SuffixCons $ weakenSuffix x


suffixIndexIs : Suffix xs ys -> IndexIs i xs v -> (j : _ ** IndexIs j ys v)
suffixIndexIs SuffixRefl p = (_ ** p)
suffixIndexIs (SuffixCons sfx) p =
    let (sfx' ** p') = suffixIndexIs sfx p
    in (FS sfx' ** That p')
