module TestListCorresp where

import Data.List as L
import Control.Category ((>>>))
import Control.Applicative

{-@ listInsert :: { i : Int | i >= 0 } ->
                  ys : [a] ->
                  xs : [a] ->
                  { zs : [a] | zs = (take i xs ++ ys ++ drop i xs) }
       @-}
       
listInsert :: Int -> [a] -> [a] -> [a]
listInsert i ys xs
  | i >= 0 = take i xs ++ ys ++ drop i xs

{-@ listDelete :: { i : Int | i >= 0 } ->
                  { n : Int | n >= 0 } ->
                  xs : [a] ->
                  { ys : [a] | ys = (take i xs ++ drop (i+n) xs) }
          @-}
          
listDelete  :: Int -> Int -> [a] -> [a]
listDelete i n
  | i >= 0 && n == 0 =  id
  | i >= 0 && n > 0 = liftA2 (++) (take i) (drop (i+n))
  
{-@ listReport :: { i : Int | i >= 0 } ->
                  { n : Int | n >= 0 } ->
                  xs : [a] ->
                  { ys : [a] | ys = (drop i . take (i+n) $ xs) }
                  @-}
                  
listReport  :: Int -> Int -> [a] -> [a]
listReport i n
  | i >= 0 && n == 0 = const []
  | i >= 0 && n > 0 = take (i+n) >>> drop i


