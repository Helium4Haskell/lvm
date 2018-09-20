module Lvm.Core.Analyses.Constraints where

import Data.Set (Set)
--import qualified Data.Set as Set

type Constraints ann t ts env = Set (Constraint ann t ts env)
data Constraint ann t ts env = Eq ann ann -- phi1 == phi2
                | EqPlus ann ann ann -- phi1 == phi2 (+) phi3
                | EqUnion ann ann ann -- phi1 == phi2 (U) phi3
                | EqTimes ann ann ann -- phi1 == phi2 (*) phi3
                | EqCond ann ann ann -- phi1 == phi2 |> phi3
                | EqT t t -- t1 == t2
                | EqTs ts ts -- ts1 == ts2
                | InstEq ts ts -- inst(ts1) == t2
                | GenEq (t, (Constraints ann t ts env), env) ts -- gen(t1(upsilon1, delta1), constraints, gamma) == ts2(upsilon2, delta2)
    deriving (Show, Eq, Ord)