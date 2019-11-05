module CodeGen.Type where

import SpirV.Options
import qualified SpirV.Type as SV
import qualified Core.Type as C


instance Conversion C.VarType SV.VarType where
    convert C.TFunc = error "Function types not supported yet"
    convert C.TInt = SV.TInt 32 Signed
    convert (C.TPair t1 t2) = SV.TStruct [convert t1, convert t2]

instance Conversion C.FuncType SV.FuncType where
    convert (C.FuncType ret params) = SV.FuncType (convert ret) (convert <$> params)
