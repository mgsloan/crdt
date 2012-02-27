{-# LANGUAGE 
    ConstraintKinds
  , FlexibleInstances
  , NoMonomorphismRestriction
  , TypeFamilies
  , UndecidableInstances
  #-}

module Data.CRDT.Classes where

import Data.CRDT.Utils

import qualified Data.Set as S
import qualified Data.IntSet as IS
import qualified Data.Map as M
import qualified Data.IntMap as IM


class Size a where
  size :: a -> Int
  null :: a -> Bool
  null = (0 ==) . size

class Size a => Clear a where
  clear :: a -> a

class Function a where
  type Domain a :: *
  type Codomain a :: *
  value :: a -> Domain a -> Codomain a

class Function a => Update a where
  update :: Domain a -> Codomain a -> a -> a
  update x y = alter (const y) x
  alter :: (Codomain a -> Codomain a) -> Domain a -> a -> a
  alter f x s = update x (f $ value s x) s

type SetLike a = (Function a, Codomain a ~ Bool)



-- Aliases

member, notMember :: SetLike a => Domain a -> a -> Bool
member = flip value
notMember = not .: member

union = join
intersection = meet
empty = bottom

isSubsetOf = leq
isProperSubsetOf x y = (x `leq` y) && not (y `leq` x)

(\\) = diff

add    = (`update` True)
delete = (`update` bottom)



-- Set instances

instance Size (S.Set a) where
  size = S.size
  null = S.null

instance Clear (S.Set a) where
  clear = const S.empty

instance Ord a => Function (S.Set a) where
  type   Domain (S.Set a) = a
  type Codomain (S.Set a) = Bool
  value = flip S.member

instance Ord a => Update   (S.Set a) where
  update x True  = S.insert x
  update x False = S.delete x



-- Map instances

instance Size (M.Map k a) where
  size = M.size
  null = M.null

instance Ord k => Clear (M.Map k a) where
  clear = const M.empty

instance Ord k => Function    (M.Map k a) where
  type   Domain (M.Map k a) = k
  type Codomain (M.Map k a) = Maybe a
  value = flip M.lookup

instance Ord k => Update      (M.Map k a) where
  alter = M.alter



-- IntSet instances

instance Function IS.IntSet where
  type   Domain IS.IntSet = Int
  type Codomain IS.IntSet = Bool
  value = flip IS.member

instance Size     IS.IntSet where
  size   = IS.size
  null   = IS.null

instance Clear    IS.IntSet where
  clear = const IS.empty

instance Update   IS.IntSet where
  update x True  = IS.insert x
  update x False = IS.delete x



-- IntMap instances

instance Size (IM.IntMap a) where
  size = IM.size
  null = IM.null

instance Clear (IM.IntMap a) where
  clear = const IM.empty

instance Function (IM.IntMap a) where
  type   Domain (IM.IntMap a) = Int
  type Codomain (IM.IntMap a) = Maybe a
  value = flip IM.lookup

instance Update (IM.IntMap a) where
  update k (Just x) = IM.insert k x
  update k Nothing  = IM.delete k
  alter  = IM.alter


-- Function instances

instance Enumerable k => Size (k -> Bool) where
  size f = length . filter f $ universe
  null f = not    . any    f $ universe

instance Enumerable k => Clear (k ->  Bool) where
  clear _ _ = False

instance Function (k -> v) where
  type   Domain (k -> v) = k
  type Codomain (k -> v) = v
  value = ($)

instance Eq k => Update (k -> v) where
  update k v f x
    | k == x = v
    | otherwise = f x
  alter g k f x
    | k == x = g $ f x
    | otherwise = f x