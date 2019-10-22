module Type where

import Syntax

data IctType
    = TInt
    | TFunc IctType IctType

type TypeCheck = Either TypeError IctType

type TypeEnv = [(Identifier, IctType)]
