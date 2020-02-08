module Ictora.Core.ToG

import Ictora.Core.Lang
import Ictora.G.Lang

%default total


convertFunc : (f : CExpr [] globals t)
           -> NoInnerLam f
           -> (t' : _ ** globals' : _ ** GFunction globals' t')
convertFunc (CGlobalVar y) (NoneAtAll x) = ?convertFunc_rhs_1
convertFunc (CLit y) (NoneAtAll x) = ?convertFunc_rhs_2
convertFunc (CApp y z) (NoneAtAll x) = ?convertFunc_rhs_3
convertFunc (CLam y) (OuterLam x) = ?convertFunc_rhs_8
convertFunc (CLet y z) (NoneAtAll x) = ?convertFunc_rhs_5


eatOuterLams : (e : CExpr locals globals t)
            -> NoInnerLam e
            -> (argTys : List 
