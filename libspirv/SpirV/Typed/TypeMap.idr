module SpirV.Type.TypeMap

import SpirV.Typed.Type
import SpirV.Raw.Misc

%access public export

TypeMap : Type
TypeMap = List ((a : TypeKind ** VarType a), Id)

lookupType : VarType a -> TypeMap -> Maybe Id
lookupType key [] = Nothing
lookupType key (((_ ** x), id) :: xs) = case key `varTypeEq` x of
                                           True => Just id
                                           False => lookupType key xs

insertType : {tk : TypeKind} -> VarType tk -> Id -> TypeMap -> TypeMap
insertType {tk} vt ident tm = ((tk ** vt), ident) :: tm
