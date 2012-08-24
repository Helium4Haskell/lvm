{------------------------------------------------------------------------
  The Core Assembler.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
------------------------------------------------------------------------}

--  $Id$

module Lvm.Common.IdMap( module Lvm.Common.Id
            , IdMap

            -- essential: used by "Asm" and "Lvm"
            , emptyMap, singleMap, elemMap, mapMap
            , insertMap, extendMap, insertMapWith
            , lookupMap, findMap
            , filterMap
            , listFromMap
            , mapMapWithId, unionMap, unionMapWith
            , updateMap

            -- exotic: used by core compiler
            , foldMap, deleteMap
            , filterMapWithId
            , mapFromList
            , unionMaps, diffMap, unionlMap
            , foldMapWithId
            , isEmptyMap, sizeMap

            ) where

import Data.Maybe
import qualified Data.IntMap as IntMap
import Lvm.Common.Id( Id, intFromId, idFromInt
         , NameSupply, splitNameSupply
         )

----------------------------------------------------------------
-- IdMap
----------------------------------------------------------------
newtype IdMap a = IdMap (IntMap.IntMap a)

emptyMap :: IdMap a
emptyMap = IdMap (IntMap.empty)

singleMap :: Id -> a -> IdMap a
singleMap x a = insertMap x a emptyMap

isEmptyMap :: IdMap a -> Bool
isEmptyMap (IdMap m) = IntMap.null m


elemMap :: Id -> IdMap a -> Bool
elemMap x (IdMap m)
  = IntMap.member (intFromId x) m

mapMap :: (a -> b) -> IdMap a -> IdMap b
mapMap f (IdMap m)
  = IdMap (IntMap.map f m)

mapMapWithId :: (Id -> a -> b) -> IdMap a -> IdMap b
mapMapWithId f (IdMap m)
  = IdMap (IntMap.mapWithKey (\i x -> f (idFromInt i) x) m)


insertMap :: Id -> a -> IdMap a -> IdMap a
insertMap x a (IdMap m)
  = IdMap (IntMap.insertWith err (intFromId x) a m)
  where
    err _ _ = error ("IdMap.insertMap: duplicate id " ++ show x)

insertMapWith :: Id -> a -> (a -> a) -> IdMap a -> IdMap a
insertMapWith x a f (IdMap m)
  = IdMap (IntMap.insertWith (const f) (intFromId x) a m)

updateMap :: Id -> a -> IdMap a -> IdMap a
updateMap x a (IdMap m)
  = IdMap (IntMap.insertWith const (intFromId x) a m)

deleteMap :: Id -> IdMap a -> IdMap a
deleteMap x(IdMap m)
  = IdMap (IntMap.delete (intFromId x) m)

extendMap :: Id -> a -> IdMap a -> IdMap a
extendMap x a (IdMap m)
  = IdMap (IntMap.insertWith const (intFromId x) a m)

lookupMap :: Id -> IdMap a -> Maybe a
lookupMap x (IdMap m)
  = IntMap.lookup (intFromId x) m

filterMap :: (a -> Bool) -> IdMap a -> IdMap a
filterMap p (IdMap m)
  = IdMap (IntMap.filter p m)

filterMapWithId :: (Id -> a -> Bool) -> IdMap a -> IdMap a
filterMapWithId p (IdMap m)
  = IdMap (IntMap.filterWithKey (\i x -> p (idFromInt i) x) m)

findMap :: Id -> IdMap a -> a
findMap x = fromMaybe (error msg) . lookupMap x
 where  
   msg = "IdMap.findMap: unknown identifier " ++ show x

listFromMap :: IdMap a -> [(Id,a)]
listFromMap (IdMap idmap)
  = map (\(i,x) -> (idFromInt i,x)) (IntMap.toList idmap)

mapFromList :: [(Id,a)] -> IdMap a
mapFromList = IdMap . IntMap.fromList . map (\(x,y) -> (intFromId x,y))

diffMap :: IdMap a -> IdMap a -> IdMap a
diffMap (IdMap map1) (IdMap map2)
  = IdMap (IntMap.difference map1 map2)

unionMap :: IdMap a -> IdMap a -> IdMap a
unionMap (IdMap map1) (IdMap map2)
  = IdMap (IntMap.unionWith err map1 map2)
  where
    err _ _   = error "IdMap.unionMap: duplicate identifiers"

unionMapWith :: (a->a->a) -> IdMap a -> IdMap a -> IdMap a
unionMapWith f (IdMap map1) (IdMap map2)
  = IdMap (IntMap.unionWith (\x y -> f x y) map1 map2)

unionlMap :: IdMap a -> IdMap a -> IdMap a
unionlMap (IdMap map1) (IdMap map2) = IdMap (IntMap.union map1 map2)

unionMaps :: [IdMap a] -> IdMap a
unionMaps = foldr unionMap emptyMap

foldMapWithId :: (Id -> a -> b -> b) -> b -> IdMap a -> b
foldMapWithId f z (IdMap m) = IntMap.foldWithKey (f . idFromInt) z m

foldMap :: (a -> b -> b) -> b -> IdMap a -> b
foldMap f z (IdMap m) = IntMap.fold f z m

sizeMap :: IdMap a -> Int
sizeMap (IdMap m) = IntMap.size m
