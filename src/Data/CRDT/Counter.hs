{-# LANGUAGE 
    FlexibleInstances
  , MultiParamTypeClasses
  , GeneralizedNewtypeDeriving
  , TemplateHaskell
  , TypeOperators
  #-}
-- | CRDT Counter
module Data.CRDT.Counter ( Inc, increment, incCount, Counter, count ) where

import Data.CRDT.Utils

newtype Inc a = Inc (Max a) deriving
  ( Bounded, Eq, Ord, Read, Show, JoinSemiLattice, Semigroup, PartialOrd )

--TODO: making newtype instances leaves the object open to violating CRDT. allow?
$(mkNewType ''Inc)

instance (Num a, PartialOrd a) => BoundedJoinSemiLattice (Inc a) where
  bottom = Inc $ Max 0

instance (Num a, PartialOrd a) => Monoid (Inc a) where { mappend = join; mempty = bottom }

increment :: (Enum a, PartialOrd a) => Inc a -> Inc a
increment = Inc `over` (Max `over` succ)

incCount :: PartialOrd a => Inc a :-> a
incCount = lens getter setter
 where
  getter = unpack . unpack
  setter x = Inc `over` join (Max x)


newtype Counter a = Counter (Inc a, Inc a) deriving
  ( Eq, Read, Show, JoinSemiLattice, BoundedJoinSemiLattice, Semigroup, Monoid )

-- TODO: Storable

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