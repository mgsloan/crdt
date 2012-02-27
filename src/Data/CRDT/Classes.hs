{-# LANGUAGE 
    FlexibleInstances
  , NoMonomorphismRestriction
  , TypeFamilies
  , UndecidableInstances
  #-}

module Data.CRDT.Classes where

import Prelude hiding (null)

import Algebra.Enumerable (Enumerable(..))
import Control.Arrow ((***))

import qualified Data.Set as S
import qualified Data.IntSet as IS
import qualified Data.Map as M
import qualified Data.IntMap as IM


-- type SetLike a = (Function a, Codomain a ~ Bool)

member, notMember :: (Function a, Codomain a ~ Bool) => Domain a -> a -> Bool
member = flip value
notMember x = not . member x

add    = (`update` True)
delete = (`update` zero)

compose :: (Function f, Function (g b), Domain f ~ b, Functor g)
        => f -> g b -> g (Codomain f)
compose = fmap . value


class Function a where
  type Domain a :: *
  type Codomain a :: *
  value :: a -> Domain a -> Codomain a

instance Function (k -> v) where
  type   Domain (k -> v) = k
  type Codomain (k -> v) = v
  value = ($)

instance Ord a => Function (S.Set a) where
  type   Domain (S.Set a) = a
  type Codomain (S.Set a) = Bool
  value = flip S.member

instance Function IS.IntSet where
  type   Domain IS.IntSet = Int
  type Codomain IS.IntSet = Bool
  value = flip IS.member

instance Ord k => Function (M.Map k a) where
  type   Domain (M.Map k a) = k
  type Codomain (M.Map k a) = Maybe a
  value = flip M.lookup

instance Function (IM.IntMap a) where
  type   Domain (IM.IntMap a) = Int
  type Codomain (IM.IntMap a) = Maybe a
  value = flip IM.lookup

instance (Function a, Function b) => Function (a, b) where
  type   Domain (a, b) = (  Domain a,   Domain b)
  type Codomain (a, b) = (Codomain a, Codomain b)
  value (f, g) = value f *** value g


class Function a => Update a where
  update :: Domain a -> Codomain a -> a -> a
  update x y = alter (const y) x
  alter :: (Codomain a -> Codomain a) -> Domain a -> a -> a
  alter f x s = update x (f $ value s x) s

instance Eq k => Update (k -> v) where
  update k v f x
    | k == x = v
    | otherwise = f x
  alter g k f x
    | k == x = g $ f x
    | otherwise = f x

instance Ord a => Update (S.Set a) where
  update x True  =  S.insert x
  update x False =  S.delete x

instance Update IS.IntSet where
  update x True  = IS.insert x
  update x False = IS.delete x

instance Ord k => Update (M.Map k a) where
  update k (Just x) =  M.insert k x
  update k Nothing  =  M.delete k
  alter = M.alter

instance Update (IM.IntMap a) where
  update k (Just x) = IM.insert k x
  update k Nothing  = IM.delete k
  alter  = IM.alter

instance (Update a, Update b) => Update (a, b) where
  update (x, x') (y, y') = update x y *** update x' y'


class Zero a where
  zero :: a
  null :: a -> Bool
  clear :: a -> a
  clear = const zero

instance Zero Bool where
  zero = False
  null = (==False)

instance (Enumerable k, Zero v) => Zero (k -> v) where
  zero = zero
  null f = not . any (null . f) $ universe

instance Zero (S.Set a) where
  zero =  S.empty
  null =  S.null

instance Zero IS.IntSet where
  zero = IS.empty
  null = IS.null

instance Zero (M.Map k a) where
  zero =  M.empty
  null =  M.null

instance Zero (IM.IntMap a) where
  zero = IM.empty
  null = IM.null

instance (Zero a, Zero b) => Zero (a, b) where
  zero = (zero, zero)
  null (a, b) = null a && null b


class Size a where
  size :: Integral i => a -> i

instance (Enumerable k, Zero v) => Size (k -> v) where
  size f = fromIntegral . length . filter (not . null . f) $ universe

instance Size (S.Set a) where
  size = fromIntegral .  S.size

instance Size IS.IntSet where
  size = fromIntegral . IS.size

instance Size (M.Map k a) where
  size = fromIntegral .  M.size

instance Size (IM.IntMap a) where
  size = fromIntegral . IM.size

{-
instance (Size a, Size b) => Zero (a, b) where
  zero = (zero, zero)
  null (a, b) = null a && null b
-}


{-
class (Function a, Function b, Domain a ~ Codomain b)
   => Composable a b where
  type CompositionType a b

or

class ( Function r, Function a, Function b
      , Domain r ~ Domain b, Codomain b ~ Domain a, Codomain a ~ Codomain r 
      ) => Composable a b r where
  compose :: a -> b -> CompositionType a b

instance (b ~ Codomain a) => Composable (b -> c) a (Domain a -> c) where
  compose f g = f $ value g

instance Composable (b -> c) a (Domain a -> c) where
-}