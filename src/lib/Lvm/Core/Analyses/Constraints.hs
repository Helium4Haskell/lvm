module Lvm.Core.Analyses.Constraints where

import Data.Set (Set)
import qualified Data.Set as Set

type Constraints ann t ts env = Set (Constraint ann t ts env)
data Constraint ann t ts env = Eq ann ann -- phi1 == phi2
                | EqPlus ann ann ann -- phi1 == phi2 (+) phi3
                | EqUnion ann ann ann -- phi1 == phi2 (U) phi3
                | EqTimes ann ann ann -- phi1 == phi2 (*) phi3
                | EqCond ann ann ann -- phi1 == phi2 |> phi3
                | EqT t t -- t1 == t2
                | EqTs ts ts -- ts1 == ts2
                | InstEq ts t -- inst(ts1) == t2
                | GenEq (t, (Constraints ann t ts env), env) ts -- gen(t1(upsilon1, delta1), constraints, gamma) == ts2(upsilon2, delta2)
    deriving (Show, Eq, Ord)


EqPlusT, EqUnionT :: t -> t -> t -> Constraints ann t ts env
-- t1 == t2 (+) t3
EqPlusT (TAnn2 t1 (use1,demand1)) (TAnn2 t2 (use2,demand2)) (TAnn2 t3 (use3,demand3)) = Set.unions
    [   EqPlusT t1 t2 t3
    ,   EqPlus use1 use2 use3
    ,   EqPlus demand1 demand2 demand3
    ]
EqPlusT (TAnn1 t1 use1) (TAnn1 t2 use2) (TAnn1 t3 use3) = Set.EqUnionTs
    [   EqPlusT t1 t2 t3
    ,   EqPlus use1 use2 use3
    ]
EqPlusT t1 t2 t3 = Set.fromList
    [   EqT t1 t2
    ,   EqT t1 t3
    ]
-- t1 == t2 (U) t3
EqUnionT (TAnn2 t1 (use1,demand1)) (TAnn2 t2 (use2,demand2)) (TAnn2 t3 (use3,demand3)) = Set.unions
    [   EqUnionT t1 t2 t3
    ,   EqUnion use1 use2 use3
    ,   EqUnion demand1 demand2 demand3
    ]
EqUnionT (TAnn1 t1 use1) (TAnn1 t2 use2) (TAnn1 t3 use3) = Set.EqUnionTs
    [   EqUnionT t1 t2 t3
    ,   EqUnion use1 use2 use3
    ]
EqUnionT t1 t2 t3 = Set.fromList
    [   EqT t1 t2
    ,   EqT t1 t3
    ]
EqPlusTs, EqUnionTs :: ts-> ts -> ts -> Constraints ann t ts env
-- ts1 == ts2 (+) ts3
EqPlusT (TAnn2 t1 (use1,demand1)) (TAnn2 t2 (use2,demand2)) (TAnn2 t3 (use3,demand3)) = Set.unions
    [   EqPlusT t1 t2 t3
    ,   EqPlus use1 use2 use3
    ,   EqPlus demand1 demand2 demand3
    ]
EqPlusT (TAnn1 t1 use1) (TAnn1 t2 use2) (TAnn1 t3 use3) = Set.EqUnionTs
    [   EqPlusT t1 t2 t3
    ,   EqPlus use1 use2 use3
    ]
EqPlusTs ts1 ts2 ts3 = Set.fromList
    [   EqTs ts1 ts2
    ,   EqTs ts1 ts3
    ]
-- ts1 == ts2 (+) ts3
EqUnionTs ts1 ts2 ts3 = Set.fromList
    [   EqTs ts1 ts2
    ,   EqTs ts1 ts3
    ]
EqTimesT, EqCondT :: t -> ann -> t -> Constraints ann t ts env
-- t1 == ann (*) t2
EqTimesT t1 _ t2 = Set.fromList
    [   EqT t1 t2
    ]
-- t1 == ann |> t2
EqCondT t1 _ t2 = Set.fromList
    [   EqT t1 t2
    ]
EqTimesTs, EqCondTs :: ts -> ann -> ts -> Constraints ann t ts env
-- ts1 == ann (*) ts2
EqTimesTs ts1 ann ts2 = Set.fromList
    [   EqTs ts1 ts2
    ]
-- ts1 == ann |> ts2
EqCondTs ts1 ann ts2 = Set.fromList
    [   EqTs ts1 ts2
    ]