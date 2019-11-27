module SpirV.Raw.Misc

%access public export

-- Identifiers are strings in SpirV
Id : Type
Id = String

-- 
data Literal = IntLit Int | FloatLit Double | BoolLit Bool
