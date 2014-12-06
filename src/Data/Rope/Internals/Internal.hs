{-# LANGUAGE PackageImports, NamedFieldPuns, MultiWayIf #-} 
module Data.Rope.Internals.Internal (
 Pos, newPos, fromPos,
 Ropeable(..), Rope,
 empty, singleton, cons,
 null, length, depth,
 toRope, -- build a non-trivial rope from a Ropeable
 fromRope,        
 index,
 insert, delete, report,
 append, splitAt,
 fromList,  -- build a non-trivial rope from a list of Ropeables
 toList,        
) where


import Prelude hiding (null, length, splitAt, concat)
import qualified Data.List as L
import qualified Data.Foldable as F
import Data.Monoid
import qualified "dlist" Data.DList as D

import Data.Rope.Internals.Types

------------------------------------------------------------------------------

chunkSingleton :: Ropeable a => Item a -> a
chunkSingleton item = chunkCons item mempty

chunkAppend :: Ropeable a => a -> a -> a
chunkAppend = mappend

chunkSlice :: Ropeable a => Pos -> Pos -> a -> a
chunkSlice i n chk = mid
     where
       (pre, temp) = chunkSplitAt i chk
       (mid, post) = chunkSplitAt n temp


chunkDelete :: Ropeable a => Pos -> Pos -> a -> a
chunkDelete i n chk = pre `mappend` post
     where
       (pre, temp) = chunkSplitAt i chk
       (mid, post) = chunkSplitAt n temp
       
------------------------------------------------------------------------------

toRope :: Ropeable a => a -> Rope a
toRope = fromList . chunkSegments

fromRope :: Ropeable a => Rope a -> a
fromRope = F.foldMap id . toList

------------------------------------------------------------------------------

empty :: Ropeable a => Rope a
empty = Nil

null :: Rope a -> Bool
null Nil = True
null _ = False

singleton :: Ropeable a => a -> Rope a
singleton x = Leaf (chunkLength x) x

cons :: (Ropeable a) => Item a -> Rope a -> Rope a
cons item Nil = singleton . chunkSingleton $ item
cons item (Leaf w chunk) = Leaf (w `add` newPos 1) (chunkCons item chunk)
cons item (Node l w r) = Node (cons item l) (w `add` newPos 1) r

length :: Ropeable a => Rope a -> Pos
length Nil = newPos 0
length (Leaf l _) = l
length (Node _ w right) = w `add` length right

depth :: Ropeable a => Rope a -> Pos
depth Nil = newPos 0
depth Leaf {} = newPos 1
depth (Node l _ r) = newPos 1 `add` (depth l `max` depth r)

------------------------------------------------------------------------------

index :: Ropeable a => Pos -> Rope a -> Maybe (Item a)
index _ Nil = Nothing
index i (Leaf l x)
        | i < l = chunkAt i x
        | otherwise = Nothing

index i (Node left w right)
        | i < w = index i left
        | otherwise = index (i `sub` w) right
        
------------------------------------------------------------------------------

append :: Ropeable a => Rope a -> Rope a -> Rope a
append Nil r = r
append l Nil = l
append l @ (Leaf w x) r = Node l w r
append l @ Node {} r = Node l (length l) r

------------------------------------------------------------------------------

splitAt :: Ropeable a => Pos -> Rope a -> Maybe (Rope a, Rope a)
splitAt i Nil = Nothing
splitAt i leaf @ (Leaf w x)
        | i < w = Just (Leaf (chunkLength l) l, Leaf (chunkLength r) r)
        | otherwise = Just (leaf, Nil)
        where (l, r) = chunkSplitAt i x
              
splitAt i node @ (Node l w r) = case compare i w of
        EQ -> Just (l, r)
        GT -> case splitAt (i `sub` w) r of
                       Nothing -> Just (node, Nil)
                       Just (r1, r2) -> Just (l `append` r1, r2)
        LT -> case splitAt i l of
                       Nothing -> Just (Nil, node)
                       Just (l1, l2) -> Just (l1, l2 `append` r)
                       
------------------------------------------------------------------------------
                       
insert :: Ropeable a => Pos -> a -> Rope a -> Rope a
insert i x Nil = toRope x
-- insert i x node @ (Leaf {}) = singleton x `append` node
insert i x rope = case splitAt i rope of
                       Just (l, r) -> l `append` toRope x `append` r

------------------------------------------------------------------------------
                       
delete :: (Eq a, Ropeable a) => Pos -> Pos -> Rope a -> Rope a
delete i n Nil = Nil

delete i n (Leaf w chk)
  | i == newPos 0 && (i `add` n) >= w = Nil
  | otherwise = Leaf w' chk'
        where
                chk' = chunkDelete i n chk
                w' = chunkLength chk'

delete i n (Node l w r)
        | l' == Nil && r' == Nil = Nil
        | l' == Nil = Node r' (length r') Nil
        | otherwise = Node l' (length l') r'
   where
           l' = if | i == newPos 0 && (i `add` n) >= w -> Nil
                   | i < w -> delete i n l
                   | otherwise -> l
           r' = if (i `add` n) > w
                   then let i' = (i `sub` w)
                            n' = ((i `add` n) `sub` w) `sub` i'
                        in delete i' n' r
                   else r

------------------------------------------------------------------------------
           
report :: Ropeable a => Pos -> Pos -> Rope a -> a
report i n Nil = mempty
report i n (Leaf len chk)
  = chunkSlice i n chk
        
report i n (Node l w r) = leftChunk `mappend` rightChunk
   where
     leftChunk = if i < w
                    then report i n l
                    else mempty
     rightChunk = if (i `add` n) > w
                     then let i' = (i `sub` w)
                              n' = ((i `add` n) `sub` w) `sub` i'
                          in report i' n' r
                     else mempty     

------------------------------------------------------------------------------
                     
instance (Ropeable a) => Monoid (Rope a) where
  mempty = empty
  mappend = append
  
  mconcat [] = empty
  mconcat [r1] = r1
  -- build a non trivial rope by merging two at a time
  mconcat (r1:r2:rest) = case rest of
                            [] -> r1 <> r2
                            _ -> mconcat [r1 <> r2, mconcat rest]

------------------------------------------------------------------------------
                            
fromList :: (Ropeable a) => [a] -> Rope a
fromList = mconcat . L.map singleton

toDList :: (Ropeable a) => Rope a -> D.DList a
toDList Nil = D.empty
toDList (Leaf _ a) = D.singleton a
toDList (Node l _ r) = toDList l `D.append` toDList r

toList :: (Ropeable a) => Rope a -> [a]
toList = D.toList . toDList

------------------------------------------------------------------------------

{-
instance Foldable (Rope a) where
   foldMap f = foldMap f . toList
   -}