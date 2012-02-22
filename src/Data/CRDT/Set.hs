{-# LANGUAGE 
    FlexibleInstances
  , MultiParamTypeClasses
  , GeneralizedNewtypeDeriving
  , TemplateHaskell
  #-}
module Data.CRDT.Set where

import Data.CRDT.Utils

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector.Storable as V

-- "G set" - growable set

newtype GSet a = GSet (S.Set a) deriving 
  ( Eq, Read, Show, JoinSemiLattice, BoundedJoinSemiLattice, PartialOrd )

$(mkNewType ''GSet)

memberGSet :: Ord a => a -> GSet a -> Bool
memberGSet x = S.member x . unpack 

insertGSet :: Ord a => a -> GSet a -> GSet a
insertGSet x = GSet `over` S.insert x



-- "2P set" - set with deletions where new need to have never been in the set

newtype Set2P a = Set2P (S.Set a, S.Set a) deriving
  ( Eq, Read, Show, JoinSemiLattice, BoundedJoinSemiLattice, PartialOrd )

$(mkNewType ''Set2P)

memberSet2P :: Ord a => a -> Set2P a -> Bool
memberSet2P x = uncurry (&&) . (S.member x *** S.notMember x) . unpack

--TODO: report success when not in deleted set?
insertSet2P :: Ord a => a -> Set2P a -> Set2P a
insertSet2P x = Set2P `over` first (S.insert x)

deleteSet2P :: Ord a => a -> Set2P a -> Set2P a
deleteSet2P x s@(Set2P (a, d))
  | x `S.member` a = Set2P (a, S.insert x d)
  | otherwise = s



--TODO: operational USet

-- Sets that provide preferential ordering to operations: Last-Writer-Wins
newtype LWWSet t a = LWWSet (M.Map a t, M.Map a t) deriving
  ( Eq, Read, Show, JoinSemiLattice, BoundedJoinSemiLattice, PartialOrd )

$(mkNewType ''LWWSet)

memberLWWSet :: (Ord t, Ord a) => a -> LWWSet t a -> Bool
memberLWWSet x = uncurry (helper `on` M.lookup x) . unpack
 where
  helper (Just _) Nothing = True
  helper (Just t) (Just t') = t' < t
  helper _ _ = False

--TODO: consider take a "Tagged"?
insertLWWSet :: Ord a => t -> a -> LWWSet t a -> LWWSet t a
insertLWWSet t x = LWWSet `over` first (M.insert x t)

deleteLWWSet :: Ord a => t -> a -> LWWSet t a -> LWWSet t a
deleteLWWSet t x = LWWSet `over` second (M.insert x t)

-- TODO: multi-register can be generalized to this as well. is it useful?


-- PNSet - positive / negative set

--Observation: PNSet is merely a map to CRDT things
{-
newtype PNSet t a = PNSet (M.Map a (IndexedReplica (Cnt t))) deriving
  ( Eq, Data, Read, Show, JoinSemiLattice, BoundedJoinSemiLattice, PartialOrd )

memberPNSet :: a -> PNSet t a -> Bool
memberPNSet x = maybe False ((> 0) . getCnt) . M.lookup x . unpack
-}

--insertPNSet :: a -> PNSet t a -> PNSet t a
--insertPNSet x = `over`