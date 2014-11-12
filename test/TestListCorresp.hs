module TestListCorresp where

import Data.List as L
import Control.Category ((>>>))
import Control.Applicative

listInsert :: Int -> [a] -> [a] -> [a]
listInsert i ys xs
  | i >= 0 = take i xs ++ ys ++ drop i xs

listDelete  :: Int -> Int -> [a] -> [a]
listDelete i j
  | i >= 0 && j == i =  id
  | i >= 0 && j > i = liftA2 (++) (take i) (drop j)

listReport  :: Int -> Int -> [a] -> [a]
listReport i j
  | i >= 0 && j == i = const []
  | i >= 0 && j > i = take j >>> drop i


