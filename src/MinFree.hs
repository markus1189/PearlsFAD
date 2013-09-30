module MinFree where

import Test.QuickCheck
import qualified Data.Array as DA
import Data.List (partition,nub)

(\\) :: Eq a => [a] -> [a] -> [a]
xs \\ ys = filter (`notElem` ys) xs

minFreeSimple :: [Int] -> Int
minFreeSimple xs = head $ [0..] \\ xs

search :: DA.Array Int Bool -> Int
search = length . takeWhile id . DA.elems

checklist :: [Int] -> DA.Array Int Bool
checklist xs = DA.accumArray (||) False (0,n) $ zip (filter (<= n) xs) (repeat True)
  where n = length xs

minFreeArray :: [Int] -> Int
minFreeArray = search . checklist

prop_minFreeArrayIsCorrect :: [Positive Int] -> Bool
prop_minFreeArrayIsCorrect xs = minFreeSimple pxs == minFreeArray pxs
  where pxs = map getPositive xs

prop_DifferenceDistributes :: Eq a => [a] -> [a] -> [a] -> Bool
prop_DifferenceDistributes as bs cs = (as ++ bs) \\ cs == (as \\ cs) ++ (bs \\ cs)

prop_DifferenceAndConcat :: Eq a => [a] -> [a] -> [a] -> Bool
prop_DifferenceAndConcat as bs cs = as \\ (bs ++ cs) == (as \\ bs) \\ cs

prop_DifferenceCommutes :: Eq a => [a] -> [a] -> [a] -> Bool
prop_DifferenceCommutes as bs cs = (as \\ bs) \\ cs == (as \\ cs) \\ bs

disjoint :: Eq a => [a] -> [a] -> Bool
disjoint as bs = as \\ bs == as

prop_DifferenceForDisjoint
  :: Eq a => [a] -> [a] -> [a] -> [a] -> Property
prop_DifferenceForDisjoint as bs us vs =
  disjoint as vs && disjoint bs us ==>
  (as ++ bs) \\ (us ++ vs) ==
  (as \\ us) ++ (bs \\ vs)

minFrom :: Int -> (Int, [Int]) -> Int
minFrom a (n,xs) | n == 0 = a
                 | m == b - a = minFrom b (n-m,vs)
                 | otherwise = minFrom a (m,us)
  where (us,vs) = partition (< b) xs
        b = a + 1 + n `div` 2
        m = length us

minFreeConquer :: [Int] -> Int
minFreeConquer xs = minFrom 0 (length nxs, nxs)
  where nxs = nub xs

prop_DivideAndConquerVersionIsCorrect :: [Positive Int] -> Bool
prop_DivideAndConquerVersionIsCorrect xs = minFreeSimple pxs == minFreeConquer pxs
  where pxs = map getPositive xs
