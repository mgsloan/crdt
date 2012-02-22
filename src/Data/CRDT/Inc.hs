{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , TemplateHaskell
  , TypeOperators
  #-}

-- | CRDT incrementor
module Data.CRDT.Inc (Inc, increment, incCount) where

import Data.CRDT.Utils

import qualified Data.Vector.Storable as V

newtype Inc a = Inc (Max a) deriving
  ( Bounded, Eq, Ord, Read, Show, V.Storable, JoinSemiLattice, Semigroup, PartialOrd )

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