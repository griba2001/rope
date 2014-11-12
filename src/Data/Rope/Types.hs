{-# LANGUAGE TypeFamilies #-}
module Data.Rope.Types where

import Data.Monoid

-- non-negative index
newtype Pos = Pos Int deriving (Eq, Ord, Show)

newIdx :: Int -> Maybe Pos
newIdx i
        | i >= 0 = Just . Pos $ i
        | otherwise = Nothing

fromPos :: Pos -> Int        
fromPos (Pos v) = v

add, sub :: Pos -> Pos -> Pos
add (Pos x) (Pos y) = Pos (x+y)
sub (Pos x) (Pos y)
        | x >= y = Pos (x - y)
        | otherwise = Pos 0

instance Bounded Pos where
   minBound = Pos 0
   maxBound = Pos maxBound
------------------------------------------------------------------------------

chunkSize = 3 :: Int

class Monoid a => Ropeable a where
  type Item a

  chunkLength :: a -> Pos

  chunkAt :: Pos -> a -> Item a

  chunkCons :: Item a -> a -> a

  chunkSplitAt :: Pos -> a -> (a, a)

  chunkSegments :: a -> [a]

------------------------------------------------------------------------------
data Rope a = Nil | Leaf {weight:: Pos, chunk :: a}
                  | Node {left :: Rope a, weight :: Pos, right :: Rope a }
                    deriving (Eq, Show)
