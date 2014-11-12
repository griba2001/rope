{-# LANGUAGE PackageImports #-}
module TestRope where

import qualified Data.List as L
import qualified Data.Rope.Rope as R
import qualified Data.Rope.Internal as I
import qualified Data.Rope.Types as T
import Control.Monad
import Test.QuickCheck.Gen
import Test.QuickCheck
import TestListCorresp
import "random" System.Random


(.$) :: a -> (a -> b) -> b
(.$) = flip ($)

default (Int, Float)


propToRopeFromRopeIsId :: [String] -> Bool
propToRopeFromRopeIsId xs = L.all (\x -> (R.unpack . R.pack) x == x) xs

propAppend :: [(String,String)] -> Bool
propAppend pairs = L.all test pairs
  where
    test (x, y) = actual == expected
      where
                actual = R.unpack $ R.pack x `R.append` R.pack y
                expected = x ++ y

data StrPairWithIdx = StrPairWithIdx String String Int deriving (Eq, Show)
data StrWithTwoIdx = StrWithTwoIdx String Int Int deriving (Eq, Show)

instance Arbitrary StrPairWithIdx where
  arbitrary = sized $ \n -> do
       k <- choose (0, n)
       i <- if k == 0 then return 0 else choose (0, k-1)
       list1 <- sequence [ arbitrary | _ <- [1..k] ]
       list2 <- sequence [ arbitrary | _ <- [1..k] ]
       return $ StrPairWithIdx list1 list2 i

instance Arbitrary StrWithTwoIdx where
  arbitrary = sized $ \n -> do
       k <- choose (0, n)
       i <- if k == 0 then return 0 else choose (0, k-1)
       j <- if k == 0 then return 0 else choose (i,k-1)
       list <- sequence [ arbitrary | _ <- [1..k] ]
       return $ StrWithTwoIdx list i j
       
propInsert :: [StrPairWithIdx] -> Int -> Bool
propInsert list rndInt = L.all test list
  where
    test (StrPairWithIdx x y i) = actual == expected
      where
        expected = listInsert i y x
        actual = R.unpack $ R.insert i y $ R.pack x

propDelete :: [StrWithTwoIdx] -> Int -> Bool
propDelete list rndInt = L.all test list
  where
    test (StrWithTwoIdx x i j) = actual == expected
      where
        expected = listDelete i j x
        actual = R.unpack $ R.delete i j $ R.pack x

propReport :: [StrWithTwoIdx] -> Int -> Bool
propReport list rndInt = L.all test list
  where
    test (StrWithTwoIdx x i j) = actual == expected
      where
        expected = listReport i j x
        actual = R.report i j $ R.pack x
        
        