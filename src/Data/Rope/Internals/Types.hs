{-# LANGUAGE TypeFamilies #-}
module Data.Rope.Internals.Types (
  Ropeable(..), Rope(..),
  Pos, newPos, fromPos, add, sub,
) where

import Data.Monoid

-- non-negative index
newtype Pos = Pos Int deriving (Eq, Ord, Show)

{-@ newPos :: { i:Int| i >= 0} -> Pos @-}

newPos :: Int -> Pos
newPos i
   | i >= 0 = Pos i

fromPos :: Pos -> Int        
fromPos (Pos v) = v

add, sub :: Pos -> Pos -> Pos
add (Pos x) (Pos y) = Pos (x+y)
sub (Pos x) (Pos y)
        | x >= y = Pos (x - y)
        | otherwise = Pos 0

{-        
instance Bounded Pos where
   minBound = Pos 0
   maxBound = Pos maxBound
   -}
------------------------------------------------------------------------------

class Monoid a => Ropeable a where
  type Item a
  
  chunkLength :: a -> Pos

  chunkAt :: Pos -> a -> Maybe (Item a)

  chunkCons :: Item a -> a -> a

  chunkSplitAt :: Pos -> a -> (a, a)

  chunkSegments :: a -> [a]

------------------------------------------------------------------------------
data Rope a = Nil | Leaf {weight:: Pos, chunk :: a}
                  | Node {left :: Rope a, weight :: Pos, right :: Rope a }
                    deriving (Eq, Show)
