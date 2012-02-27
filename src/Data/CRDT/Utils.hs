{-# LANGUAGE 
    DeriveDataTypeable
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , NoMonomorphismRestriction
  , StandaloneDeriving
  , TemplateHaskell
  , TypeOperators
  #-}

module Data.CRDT.Utils                 -- Re-Exports 
  (                                module Algebra.Enumerable
  ,                                module Algebra.Lattice
  , PartialOrd(..)                     -- Algebra.PartialOrd
  , first, second, (***)               -- Control.Arrow
  , Newtype(..), over                  -- Control.Newtype
  , mkNewType                          -- Control.Newtype.TH
  , on                                 -- Data.Function
  ,                                module Data.Label
  , comparing                          -- Data.Ord
  , Semigroup(..), Max(..), Monoid(..) -- Data.Semigroup
  , V.Storable

  , (.:), with2, over2, maxBy          -- Utilities
  , union, intersection, empty, isSubsetOf, isProperSubsetOf, (\\)
  ) where

import Algebra.Enumerable
import Algebra.Lattice
import Algebra.PartialOrd   (PartialOrd(..))
import Control.Arrow        (first, second, (***))
import Control.Newtype      (Newtype(..), over)
import Control.Newtype.TH   (mkNewType)
import Data.Align           (alignWith, alignVectorWith)
import Data.Function        (on)
import Data.Label
import Data.Ord             (comparing)
import Data.Semigroup       (Semigroup(..), Max(..), Monoid(..))
import Data.Vector.Storable ((//), (!))
import Data.These           (These(..), mergeThese)
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Storable as V

$(mkNewType ''Max)

instance PartialOrd a => PartialOrd      (Max a) where
  leq  = Max `with2` leq

instance PartialOrd a => JoinSemiLattice (Max a) where
  join = Max `over2` joinPartialOrd

joinPartialOrd :: PartialOrd a => a -> a -> a
joinPartialOrd x y
  | x `leq` y = y
  | y `leq` x = x
  | otherwise = x

deriving instance V.Storable a => V.Storable (Max a)

instance (JoinSemiLattice a, JoinSemiLattice b)
      => JoinSemiLattice (These a b) where
  join (This  a  ) (This  x  ) = This $ join a x
  join (This  a  ) (That    y) = These a          y
  join (This  a  ) (These x y) = These (join a x) y
  join (That    b) (This  x  ) = These x          b
  join (That    b) (That    y) = That             (join b y)
  join (That    b) (These x y) = These x          (join b y)
  join (These a b) (This  x  ) = These (join a x) b
  join (These a b) (That    y) = These a          (join b y)
  join (These a b) (These x y) = These (join a x) (join b y)

instance (V.Storable a, JoinSemiLattice a)
      => JoinSemiLattice (V.Vector a) where
  join = alignVectorWith (mergeThese join)

instance (V.Storable a, BoundedJoinSemiLattice a)
      => BoundedJoinSemiLattice (V.Vector a) where
  bottom = V.empty

instance (V.Storable a, PartialOrd a)
      => PartialOrd (V.Vector a) where
  leq x y = V.all id $ V.zipWith leq x y

instance (JoinSemiLattice a) => JoinSemiLattice [a] where
  join = alignWith (mergeThese join)

instance (BoundedJoinSemiLattice a) => BoundedJoinSemiLattice  [a] where
  bottom = []

instance (PartialOrd a) => PartialOrd [a] where
  leq x y = all id $ zipWith leq x y

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

with2 :: (Newtype a' a) => (a -> a') -> (a -> a -> c) -> a' -> a' -> c
with2 _ f x y = f (unpack x) (unpack y)

over2 :: (Newtype a' a) => (a -> a') -> (a -> a -> a) -> a' -> a' -> a'
over2 c f = pack .: with2 c f

maxBy :: (a -> a -> Ordering) -> a -> a -> a
maxBy f x y = case f x y of
  LT -> y
  EQ -> x
  GT -> x

atIndex :: GV.Vector v a => Int -> v a :-> a
atIndex ix = lens (GV.! ix) ( \x -> (GV.// [(ix, x)]) )

-- Aliases

union = join
intersection = meet
empty = bottom

isSubsetOf = leq
isProperSubsetOf x y = (x `leq` y) && not (y `leq` x)

(\\) = diff