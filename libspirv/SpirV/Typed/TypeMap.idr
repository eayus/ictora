module SpirV.Type.TypeMap

import SpirV.Typed.Type
import SpirV.Raw.Misc

%access public export

record TypeMap where
    constructor MkTypeMap
    varTypes : List ((a : TypeKind ** VarType a), Id)
    funcTypes : List (FuncType, Id)


dLookup : VarType a -> List ((a : TypeKind ** VarType a), b) -> Maybe b
dLookup elem [] = Nothing
dLookup elem (((_ ** key), val) :: xs) = if varTypeEq elem key then Just val else dLookup elem xs


lookupType : VarType a -> TypeMap -> Maybe Id
lookupType vt = dLookup vt . varTypes

insertType : {tk : TypeKind} -> VarType tk -> Id -> TypeMap -> TypeMap
insertType {tk} vt ident = record { varTypes $= (((tk ** vt), ident) ::) }


lookupFuncType : FuncType -> TypeMap -> Maybe Id
lookupFuncType ft = (lookup ft) . funcTypes

insertFuncType : FuncType -> Id -> TypeMap -> TypeMap
insertFuncType ft ident = record { funcTypes $= ((ft, ident) ::) }
