module Ictora.Syntax.TypeError

import Ictora.Syntax.Lang
import Ictora.Ict.Lang

%access public export


data TypeError : Type where
    UnknownType : (badName : SIdent)
               -> (typesInScope : List SIdent)
               -> TypeError

    UndefinedVariable : (badName : SIdent)
                     -> (namesInScope : List SIdent)
                     -> TypeError

    NonFunctionApplication : (badFuncTy : ITy)
                          -> (argTy : ITy)
                          -> TypeError

    ArgTypeMismatch : (expectedTy : ITy)
                   -> (gotTy : ITy)
                   -> TypeError







similarity : Eq a => List a -> List a -> Nat
similarity [] [] = Z
similarity xs [] = length xs
similarity [] ys = length ys
similarity (x :: xs) (y :: ys) =
    case x == y of
         True => similarity xs ys
         False => 1 + min (similarity xs (y :: ys))
                          (min (similarity (x :: xs) ys)
                               (similarity xs ys))


stringSim : String -> String -> Nat
stringSim x y = similarity (unpack x) (unpack y)


findSuggestions : List String -> String -> List String
findSuggestions xs x = filter (\y => stringSim x y < 5) xs


showSuggestions : List String -> String -> String
showSuggestions xs x =
    let names = findSuggestions xs x
    in if isNil names then "" else "Perhaps you meant one of " ++ (concat $ intersperse " " $ show <$> names) ++ "?\n"


implementation Show TypeError where
    show (UnknownType name expected) = "Unknown type " ++ show name ++ ".\n" ++ showSuggestions expected name
    show (UndefinedVariable name expected) = "Undefined varaible " ++ show name ++ ".\n"
    show (NonFunctionApplication badFuncTy argTy) = "Argument applied to expression that is not a function. Perhaps you applied something to too many arguments?"
    show (ArgTypeMismatch expected got) = "Function applied to argument of wrong type"
