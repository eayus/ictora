module SpirV.Typed.Builder

import Control.Monad.State
import SpirV.Raw.Program

Builder : Type -> Type
Builder = State Program


