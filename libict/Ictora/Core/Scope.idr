module Ictora.Core.Scope

import Ictora.Core.Lang

weakenProgScope : CProgram (S len) (f :: scope) -> CProgram (S $ S len) (f :: n :: scope)
weakenProgScope CEmptyProg = CEmptyProg
weakenProgScope (CConsFunc f prog) = CConsFunc (weakenFuncScope f) (weakenProgScope prog)
