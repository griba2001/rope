module Data.Rope.Rope (
  empty, cons,
  null, length,
  pack, unpack,
  insert, delete,
  append, report,
  splitAt,        
) where

import Prelude hiding (null, length, splitAt)
import qualified Data.Rope.Internals.Internal as I
import Data.Rope.Instances()
import Data.Rope.Internals.Types
import Data.Monoid

empty :: (Ropeable a) => Rope a
empty = I.empty

cons :: (Ropeable a) => Item a -> Rope a -> Rope a
cons = I.cons

null :: (Ropeable a) => Rope a -> Bool
null = I.null

length :: (Ropeable a) => Rope a -> Int
length = fromPos . I.length

pack :: (Ropeable a) => a -> Rope a
pack = I.toRope

unpack :: (Ropeable a) => Rope a -> a
unpack = I.fromRope

insert :: Ropeable a => Int -> a -> Rope a -> Rope a
insert i
   | i >= 0 = I.insert (Pos i)

delete :: (Eq a, Ropeable a) => Int -> Int -> Rope a -> Rope a
delete i j
  | i >= 0 && j == i = id
  | i >= 0 && j > i = I.delete (Pos i) (Pos j)

append :: Ropeable a => Rope a -> Rope a -> Rope a
append = I.append

report :: Ropeable a => Int -> Int -> Rope a -> a
report i j rope
       | i >= 0 && j == i = mempty
       | i >= 0 && j > i = I.report (Pos i) (Pos j) rope

splitAt :: Ropeable a => Int -> Rope a -> Maybe (Rope a, Rope a)
splitAt i rope
        | i == 0 = Just (empty, rope)
        | i > 0 = I.splitAt (Pos i) rope
        


