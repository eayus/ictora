module Ictora.Util

%access public export

fromJust : Maybe a -> a
fromJust (Just a) = a

lookupPred : (a -> Bool) -> List (a, b) -> Maybe b
lookupPred p [] = Nothing
lookupPred p ((x, y) :: xs) = if p x then Just y else lookupPred p xs

lookupPredJust : (a -> Bool) -> List (a, b) -> b
lookupPredJust p xs = fromJust $ lookupPred p xs
