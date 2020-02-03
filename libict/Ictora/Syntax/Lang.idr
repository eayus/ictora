module Ictora.Syntax.Lang

%access public export

infixr 2 ~>


SIdent : Type
SIdent = String


data STy : Type where
    NamedTy : SIdent -> STy
    (~>) : STy -> STy -> STy


data SExpr : Type where
    SVar : SIdent -> SExpr
    SApp : SExpr -> SExpr -> SExpr
    SLam : (var : SIdent) -> (ty : STy) -> SExpr -> SExpr
    SLet : SIdent -> SExpr -> SExpr -> SExpr
    SLit : Int -> SExpr


SProgram : Type
SProgram = List (SIdent, SExpr)





-- Test Programs

testProg1 : SProgram
testProg1 = [ ("main", SVar "x") ]

testProg2 : SProgram
testProg2 = [ ("f", SLit 5)
            , ("main", SVar "f") ]

testProg3 : SProgram
testProg3 = [ ("id", SLam "x" (NamedTy "Intt") (SVar "x")) ]
