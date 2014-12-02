{-# LANGUAGE TypeSynonymInstances, TypeFamilies, FlexibleInstances #-}
module Data.Rope.Instances.InstList where

import qualified Data.List as L

import Data.Rope.Internals.Types
import Safe

chunkSize :: Int
chunkSize = 3  -- low for test purposes


instance Ropeable [a] where
  type Item [a] = a
  
  -- chunkCons ch str = ch : str
  chunkCons = (:)
  
  chunkSplitAt i = L.splitAt . fromPos $ i
  
  chunkLength = newPos . L.length
  
  chunkAt i s = s `Safe.atMay` (fromPos i)
  
  chunkSegments [] = [[]]
  chunkSegments xs = case post of
                       [] -> [pre]   
                       _ ->  pre : chunkSegments post
    where
          (pre, post) = chunkSplitAt (newPos chunkSize) xs

  
