{-# LANGUAGE 
    ConstraintKinds
  , FlexibleInstances
  , MultiParamTypeClasses
  , GeneralizedNewtypeDeriving
  , TemplateHaskell
  , TypeFamilies
  , UndecidableInstances
  #-}
module Data.CRDT.Set where

import Data.CRDT.Classes
import Data.CRDT.Counter
import Data.CRDT.User (UserIx)
import Data.CRDT.Utils

import Prelude hiding (null)

import Control.Applicative ((<$>))
import Control.Arrow ((***))
import Data.Label
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector.Storable as V

-- Classes

--TODO: reducers?

-- "G set" - growable set

newtype GSet a = GSet (S.Set a) deriving
  ( Eq, Read, Show
  , Size, Update
  , Monoid, Semigroup
  , JoinSemiLattice, BoundedJoinSemiLattice, PartialOrd
  )

instance Ord a => Function  (GSet a) where
  type   Domain (GSet a) = a
  type Codomain (GSet a) = Bool
  value (GSet s) = value s

$(mkNewType ''GSet)




-- "2P set" - set with deletions where new need to have never been in the set.

newtype Set2P a = Set2P (a, a) deriving
  ( Eq, Read, Show
  , Monoid, Semigroup
  , JoinSemiLattice, BoundedJoinSemiLattice, PartialOrd
  )

$(mkNewType ''Set2P)

instance (Function a, Codomain a ~ Bool) => Function (Set2P a) where
  type   Domain (Set2P a) = Domain a
  type Codomain (Set2P a) = Bool
  value (Set2P (s, d)) x = member x s && notMember x d

instance (Update a, Function a, Codomain a ~ Bool) => Update (Set2P a) where
  update k = over Set2P . first . update k

instance (Zero a, JoinSemiLattice a) => Zero (Set2P a) where
  zero = Set2P zero
  clear (Set2P (s, d)) = Set2P (s, s `join` d)

instance Size a => Size (Set2P a) where
  size (Set2P (s, d)) = size s - size d

instance Lattice a => MeetSemiLattice (Set2P a) where
  meet (Set2P (s, d)) (Set2P (s', d')) = Set2P (s `meet` s', d `join` d')

instance BoundedLattice a => BoundedMeetSemiLattice (Set2P a) where
  top = Set2P (top, bottom)

instance DiffLattice a => DiffLattice (Set2P a) where
  diff (Set2P (s, d)) (Set2P (s', d'))
    = Set2P (s \\ s', s' \\ s `join` d `join` d')

instance (DiffLattice a, Monoid a) => ResiduatedLattice (Set2P a) where
  residualL = diff
  residualR = diff

instance        Lattice a =>        Lattice (Set2P a)
instance BoundedLattice a => BoundedLattice (Set2P a)



-- Set2P variant, represented as (members, deleted) instead of (seen, deleted).

data RemSet a = RemSet (a, a) deriving ( Eq, Read, Show )

$(mkNewType ''RemSet)

instance (Function a, Codomain a ~ Bool) => Function (RemSet a) where
  type   Domain (RemSet a) = Domain a
  type Codomain (RemSet a) = Bool
  value (RemSet (a, _)) = value a

instance (Update a, Function a, Codomain a ~ Bool) => Update (RemSet a) where
  update x True  s@(RemSet (a, d))
    | x `notMember` d = RemSet (update x True a, d)
    | otherwise = s
  update x False (RemSet (a, d))
    = RemSet (update x False a, update x True d)

instance (Zero a, BoundedJoinSemiLattice a) => Zero (RemSet a) where
  zero = RemSet (zero, zero)
  null = null . fst . unpack
  clear (RemSet (a, d)) = RemSet (bottom, a `join` d)

instance Size a => Size (RemSet a) where
  size = size . fst . unpack

instance (PartialOrd a) => PartialOrd (RemSet a) where
  leq (RemSet (a, d)) (RemSet (a', d')) = (a `leq` a') && (d `leq` d)

instance DiffLattice a
      => JoinSemiLattice (RemSet a) where
  join (RemSet (a, d)) (RemSet (a', d'))
    = RemSet ( (a \\ d') `join` (a' \\ d), d `join` d')

instance DiffLattice a
      => MeetSemiLattice (RemSet a) where
  meet (RemSet (a, d)) (RemSet (a', d'))
    = RemSet ( a `meet` a', joins [d, d', a \\ a', a' \\ a] )

instance (DiffLattice a, BoundedJoinSemiLattice a)
      => BoundedJoinSemiLattice (RemSet a) where
  bottom = RemSet (bottom, bottom)

instance (DiffLattice a, BoundedLattice a)
      => BoundedMeetSemiLattice (RemSet a) where
  top = RemSet (top, bottom)

instance DiffLattice a => DiffLattice (RemSet a) where
  diff (RemSet (a, d)) (RemSet (a', d'))
    = RemSet (a `diff` a', d `join` d' `join` (a' \\ a))

instance DiffLattice a => ResiduatedLattice (RemSet a) where
  residualL = diff
  residualR = diff

instance DiffLattice a =>        Lattice (RemSet a) where
instance DiffLattice a => BoundedLattice (RemSet a) where

instance DiffLattice a => Semigroup (RemSet a) where
  (<>) = join

instance DiffLattice a => Monoid (RemSet a) where
  mappend = join
  mempty = bottom


-- Observed-delete Set

newtype ORSet t a = ORSet (M.Map a (RemSet (S.Set t))) deriving
  ( Eq, Read, Show
  , JoinSemiLattice, BoundedJoinSemiLattice
  , MeetSemiLattice, BoundedMeetSemiLattice
  , Lattice,         BoundedLattice
  , ResiduatedLattice, DiffLattice )

instance (Ord a, Ord t, Enumerable t) => Monoid (ORSet t a) where
  mappend (ORSet a) (ORSet b) = ORSet $ join a b

$(mkNewType ''ORSet)

instance (Ord t, Ord a) => Function (ORSet t a) where
  type   Domain (ORSet t a) = a
  type Codomain (ORSet t a) = Bool
  value (ORSet m) = maybe False (not . null) . value m

instance (Ord t, Ord a) => Zero (ORSet t a) where
  zero = ORSet zero
  null = all null . M.elems . unpack
  clear = over ORSet $ M.map clear

instance Ord a => Size (ORSet t a) where
  size (ORSet m) = sum . map size $ M.elems m


{-

-- TODO: operational USet

-- Sets that provide preferential ordering to operations: Last-Writer-Wins
newtype LWWSet t a = LWWSet (M.Map a t, M.Map a t) deriving
  ( Eq, Read, Show, JoinSemiLattice, BoundedJoinSemiLattice, PartialOrd )

$(mkNewType ''LWWSet)

instance Ord a => LWWSet a where
  member = uncurry (helper `on` M.lookup x) . unpack
   where
    helper (Just _) Nothing = True
    helper (Just t) (Just t') = t' < t
    helper _ _ = False
  update 



--TODO: consider take a "Tagged"?
insertLWWSet :: Ord a => t -> a -> LWWSet t a -> LWWSet t a
insertLWWSet t x = LWWSet `over` first (M.update x t)

deleteLWWSet :: Ord a => t -> a -> LWWSet t a -> LWWSet t a
deleteLWWSet t x = LWWSet `over` second (M.update x t)

-- TODO: multi-register can be generalized to this as well. is it useful?



-- PNSet - positive / negative set
newtype PNSet t a = PNSet (M.Map a (V.Vector (Counter t))) deriving
  ( Eq, Read, Show, JoinSemiLattice, BoundedJoinSemiLattice, PartialOrd )

$(mkNewType ''PNSet)

instance Ord a => SetLike (GSet a) where
  type Element (GSet a) = a
  member x = maybe False ((> 0) . get count) . M.lookup x . unpack

alterPNSet :: (Counter t -> Counter t) -> UserIx -> a -> PNSet t a -> PNSet t a
alterPNSet f u x = PNSet `over` M.alter (maybe (f bottom) $ modify (atIndex u) f) x

insertPNSet, deletePNSet :: Enum t => UserIx -> a -> PNSet t a -> PNSet t a
insertPNSet = alterPN succ
deletePNSet = alterPN pred

-}