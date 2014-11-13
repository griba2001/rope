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
  
  chunkSplitAt (Pos i) = L.splitAt i
  
  chunkLength = Pos . L.length
  
  chunkAt (Pos i) s = s `Safe.at` i
  
  chunkSegments [] = [[]]
  chunkSegments xs = case post of
                       [] -> [pre]   
                       _ ->  pre : chunkSegments post
    where
          (pre, post) = chunkSplitAt (Pos chunkSize) xs

  
