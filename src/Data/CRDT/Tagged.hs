{-# LANGUAGE 
    TemplateHaskell
  , FlexibleInstances
  , MultiParamTypeClasses
  , GeneralizedNewtypeDeriving
  #-}
module Data.CRDT.Tagged where

import Data.CRDT.Inc
import Data.CRDT.Utils

import qualified Data.Vector.Storable as V

-- Register that provide preferential ordering (Last-Writer-Wins)

data Tagged t a = Tagged { tag :: t, untagged :: a } deriving
  ( Eq, Show, Read )

instance Functor (Tagged t) where
  fmap f (Tagged t a) = Tagged t $ f a

--TODO: overload others for efficiency?
instance (Ord t, Eq a) => Ord (Tagged t a) where
  compare = compare `on` tag
 
instance Ord t => JoinSemiLattice (Tagged t a) where
  join = maxBy $ comparing tag

instance (Ord t, BoundedJoinSemiLattice t, BoundedJoinSemiLattice a)
      => BoundedJoinSemiLattice (Tagged t a) where
  bottom = Tagged bottom bottom

instance (PartialOrd t, Eq a) => PartialOrd (Tagged t a) where 
  leq = leq `on` tag

-- Register that provide partial preferential ordering (Multi-Value)

newtype MultiTagged t a = MultiTagged [Tagged t a] deriving
  ( Eq, Ord, Read, Show)

$(mkNewType ''MultiTagged)

newtype VersionVector a = VersionVector (V.Vector (Inc a)) deriving
  ( Eq, Ord, Read, Show, JoinSemiLattice, BoundedJoinSemiLattice )

$(mkNewType ''VersionVector)

--TODO: what about different sizes?
instance (V.Storable a, PartialOrd a) => PartialOrd (VersionVector a) where
  leq = VersionVector `with2` (V.and .: V.zipWith leq)

instance (Eq a, PartialOrd t) => PartialOrd              (MultiTagged t a)

instance (Eq a, PartialOrd t) => JoinSemiLattice         (MultiTagged t a) where
  join = MultiTagged `over2` (\xs ys -> helper xs ys ++ helper ys xs)
   where
    helper xs = filter (\v -> all (`leq` v) xs)

instance (Eq a, PartialOrd t) => BoundedJoinSemiLattice  (MultiTagged t a) where
  bottom = MultiTagged []