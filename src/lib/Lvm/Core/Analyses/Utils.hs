module Lvm.Core.Analyses.Utils where

import Control.Monad.State.Lazy (State, evalState, get, put)

type Pi = Int
type Name = String
type Fresh a = State Int a

fresh :: Fresh Int
fresh = do
    x <- get
    put $ x + 1
    return x