module Data.Rope.Rope (
  Rope,
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

{-@ insert :: Ropeable a => {i:Int | i >= 0} ->
                           a ->
                           Rope a ->
                           Rope a
@-}

insert :: Ropeable a => Int -> a -> Rope a -> Rope a
insert i
   | i >= 0 = I.insert (newPos i)

{-@ delete :: (Eq a, Ropeable a) => { i : Int | i >= 0} -> { n : Int | n >= 0 } -> Rope a -> Rope a
@-}

delete :: (Eq a, Ropeable a) => Int -> Int -> Rope a -> Rope a
delete i n
  | i >= 0 && n == 0 = id
  | i >= 0 && n > 0 = I.delete (newPos i) (newPos n)

append :: Ropeable a => Rope a -> Rope a -> Rope a
append = I.append

{-@ report :: (Ropeable a) => { i : Int | i >= 0 } -> { n : Int | n >= 0 } -> Rope a -> a
@-}
report :: Ropeable a => Int -> Int -> Rope a -> a
report i n rope
       | i >= 0 && n == 0 = mempty
       | i >= 0 && n > 0 = I.report (newPos i) (newPos n) rope

{-@ splitAt :: (Ropeable a) => { i : Int | i >= 0 } -> Rope a -> Maybe (Rope a, Rope a)
               @-}       
splitAt :: Ropeable a => Int -> Rope a -> Maybe (Rope a, Rope a)
splitAt i rope
        | i == 0 = Just (empty, rope)
        | i > 0 = I.splitAt (newPos i) rope
        


