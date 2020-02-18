module Lang.Ict.Common

import public Data.Vect


public export
data IBasicTy
    = IIntTy
    | IBoolTy


public export
data ITy
    = INullaryTy IBasicTy
    | IArrowTy IBasicTy ITy
    | ITmplArrowTy ITy ITy
