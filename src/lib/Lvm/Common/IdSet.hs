{------------------------------------------------------------------------
  The Core Assembler.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
------------------------------------------------------------------------}

--  $Id$

-- this module is exotic, only used by the core compiler
-- but it works with any IdMap
module Lvm.Common.IdSet
   ( IdSet, Id
   , emptySet, singleSet, elemSet, filterSet, foldSet
   , insertSet, deleteSet, unionSet, unionSets, diffSet
   , listFromSet, setFromList, sizeSet, isEmptySet
   ) where

import Data.IntSet
import Data.List (sort)
import Lvm.Common.Id
import qualified Data.IntSet as IntSet

----------------------------------------------------------------
-- IdSet
----------------------------------------------------------------

newtype IdSet = IdSet IntSet

emptySet :: IdSet
emptySet = IdSet empty

singleSet :: Id -> IdSet
singleSet = IdSet . singleton . intFromId

elemSet :: Id -> IdSet -> Bool
elemSet x (IdSet s) = member (intFromId x) s

filterSet :: (Id -> Bool) -> IdSet -> IdSet
filterSet p (IdSet s) = IdSet (IntSet.filter (p . idFromInt) s)

foldSet :: (Id -> a ->  a) -> a -> IdSet -> a
foldSet f a (IdSet s) = fold (f . idFromInt) a s

insertSet :: Id -> IdSet -> IdSet
insertSet x (IdSet s) = IdSet (insert (intFromId x) s)

deleteSet :: Id -> IdSet -> IdSet
deleteSet x (IdSet s) = IdSet (delete (intFromId x) s)

unionSet :: IdSet -> IdSet -> IdSet
unionSet (IdSet s1) (IdSet s2) = IdSet (s1 `union` s2)

unionSets :: [IdSet] -> IdSet
unionSets xs = IdSet (unions [ s | IdSet s <- xs ])

diffSet :: IdSet -> IdSet -> IdSet
diffSet (IdSet s1) (IdSet s2) = IdSet (difference s1 s2)

-- sort is needed to not rely on an id's index
listFromSet :: IdSet -> [Id]
listFromSet (IdSet s) = sort [ idFromInt n | n <- elems s ]

setFromList :: [Id] -> IdSet
setFromList = foldr insertSet emptySet

sizeSet :: IdSet -> Int
sizeSet (IdSet s) = IntSet.size s

isEmptySet :: IdSet -> Bool
isEmptySet (IdSet s) = IntSet.null s