{-# LANGUAGE TypeSynonymInstances, TypeFamilies, FlexibleInstances #-}
module Data.Rope.Instances.InstString where

import qualified Data.List as L

import Data.Rope.Types
import Data.Rope.Internal
import Safe

instance Ropeable [a] where
  type Item [a] = a
  
  chunkCons ch str = ch : str
  
  chunkSplitAt (Pos i) s = L.splitAt i s
  
  chunkLength s = Pos . L.length $ s
  
  chunkAt (Pos i) s = s `Safe.at` i
  
  chunkSegments [] = [[]]
  chunkSegments xs = case post of
                       [] -> [pre]   
                       _ ->  pre : chunkSegments post
    where
          (pre, post) = chunkSplitAt (Pos chunkSize) xs

  
