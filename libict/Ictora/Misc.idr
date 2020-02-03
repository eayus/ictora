module Ictora.Misc

import Data.Vect

%access public export
%default total


Map : Type -> Type -> Type
Map a b = List (a, b)


data NoKey : a -> Map a b -> Type where
    Nil : NoKey k []
    (::) : Not (k = x) -> NoKey k xys -> NoKey k ((x, y) :: xys)


data UniqKeys : Map a b -> Type where
    UniqNil : UniqKeys []
    UniqCons : NoKey k xs -> UniqKeys xs -> UniqKeys ((k, _) :: xs)


data Lookup : a -> Map a b -> b -> Type where
    This : Lookup k ((k, v) :: m) v
    That : Not (k = x) -> Lookup k m v -> Lookup k ((x, _) :: m) v


lookupPrf : DecEq a => (k : a) -> (m : Map a b) -> Maybe (v : _ ** Lookup k m v)
lookupPrf k [] = Nothing
lookupPrf k ((x, y) :: xys) =
    case (decEq k x, lookupPrf k xys) of
         (Yes Refl, _) => Just (y ** This)
         (No kxneq, Just (v ** later)) => Just (v ** That kxneq later)
         (No kxneq, Nothing) => Nothing


decNoKey : DecEq a => (k : a) -> (m : Map a b) -> Maybe (NoKey k m)
decNoKey k [] = Just Nil
decNoKey k ((x, y) :: xys) =
    case decEq k x of
         Yes _ => Nothing
         No neq => (neq ::) <$> decNoKey k xys
