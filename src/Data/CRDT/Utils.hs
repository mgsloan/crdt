{-# LANGUAGE 
    DeriveDataTypeable
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , StandaloneDeriving
  , TemplateHaskell
  #-}

module Data.CRDT.Utils                              -- Re-Exports 
  ( JoinSemiLattice(..), BoundedJoinSemiLattice(..) -- Algebra.Lattice
  , PartialOrd(..)                                  -- Algebra.PartialOrd
  , first, second, (***)                            -- Control.Arrow
  , Newtype(..), over                               -- Control.Newtype
  , mkNewType                                       -- Control.Newtype.TH
  , on                                              -- Data.Function
  ,                                             module Data.Label
  , comparing                                       -- Data.Ord
  , Semigroup(..), Max(..), Monoid(..)              -- Data.Semigroup

  , (.:), with2, over2, maxBy                       -- Utilities
  ) where

import Algebra.Lattice      (JoinSemiLattice(..), BoundedJoinSemiLattice(..))
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