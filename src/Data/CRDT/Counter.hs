{-# LANGUAGE 
    FlexibleInstances
  , MultiParamTypeClasses
  , GeneralizedNewtypeDeriving
  , TemplateHaskell
  , TypeOperators
  #-}
-- | CRDT Counter
module Data.CRDT.Counter ( Counter, count ) where

import Data.CRDT.Inc
import Data.CRDT.Utils

newtype Counter a = Counter (Inc a, Inc a) deriving
  ( Eq, Read, Show, JoinSemiLattice, BoundedJoinSemiLattice, Semigroup, Monoid )

$(mkNewType ''Counter)

instance (Enum a, Integral a, Num a, PartialOrd a)
      => Enum (Counter a) where
  succ = Counter `over` first  increment
  pred = Counter `over` second increment
  toEnum i = Counter (set incCount (fromIntegral i) bottom, bottom)
  fromEnum = fromIntegral . get count

count :: (Num a, PartialOrd a) => Counter a :-> a
count = lens getter setter
 where
  getter = uncurry ((-) `on` get incCount) . unpack
  setter x c = pack . modifier $ unpack c
   where
    delta = x - getter c
    modifier
      | delta `leq` 0 = second $ modify incCount $ subtract delta
      | otherwise     = first  $ modify incCount $ (delta+)