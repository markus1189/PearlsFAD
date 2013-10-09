{-# LANGUAGE NoMonomorphismRestriction #-}
module Surpasser where

import Test.QuickCheck.Modifiers
import Data.List (sort)

-- In this pearl we solve a small programming exercise of Martin
-- Rem. While Rem's solution uses binary search, our solution is
-- another application of divide and conquer. By definition, a
-- surpasser of an element of an array is a greater element to the
-- right, so x[j] is a surpasser of x[i] if i<j and x[i] , x[j]. The
-- surpasser count of an element is the number of its surpassers.

-- NOTE: solution assumes sorted table version, which allows extended reasoning
msc :: Ord a => [a] -> Int
msc = maximum . map snd . table

table :: Ord a => [a] -> [(a, Int)]
table [x] = [(x,0)]
table xs = join (m-n) (table ys) (table zs)
  where m = length xs
        n = m `div` 2
        (ys,zs) = splitAt n xs

join :: (Eq b, Num b, Ord a) => b -> [(a, b)] -> [(a, b)] -> [(a, b)]
join 0 txs [] = txs
join _ [] tys = tys
join n txs@((x,c) : txs') tys@((y,d) : tys') | x < y = (x,c+n) : join n txs' tys
                                             | x >= y = (y,d) : join (n-1) txs tys'
-- not divide and conquer friendly
msc' :: Ord a => [a] -> Int
msc' xs = maximum [scount z zs | (z:zs) <- tails xs]

scount :: Ord a => a -> [a] -> Int
scount x = length . filter (x <)

scountHead :: Ord a => [a] -> Int
scountHead (x:xs) = scount x xs

-- different from Data.List.tails:
--   Data.List.tails [] = [[]]
--             tails [] =  []
tails :: [a] -> [[a]]
tails [] = []
tails (x:xs) = (x:xs) : tails xs

prop_tailsDivideConquer :: Eq a => [a] -> [a] -> Bool
prop_tailsDivideConquer as bs = tails (as ++ bs) == map (++bs) (tails as) ++ tails bs

-- inefficient version
table' :: Ord a => [a] -> [(a, Int)]
table' xs = sort [(z,scount z zs) | z:zs <- tails xs]

{-

table (xs ++ ys)

[(z,scount z zs) | z:zs <- tails (xs ++ ys)]

[(z,scount z zs) | z:zs <- map (++ys) (tails xs) ++ tails ys]

[(z,scount z (zs ++ ys)) | z:zs <- tails xs] ++
  [(z,scount z zs) | z:zs <- tails ys]

[(z,scount z zs + scount z ys) | z:zs <- tails xs] ++
  [(z,scount z zs) | z:zs <- tails ys]

[(z,c + scount z (map fst (table ys))) | (z,c) <- table xs] ++ table ys

-}

-- inefficient version
join' :: Ord a => [(a, Int)] -> [(a, Int)] -> [(a, Int)]
join' txs tys = [(z,c + tcount z tys) | (z,c) <- txs] `sortedMerge` tys

-- NOTE: own implementation, not mentioned in book
sortedMerge :: Ord a => [a] -> [a] -> [a]
sortedMerge xs [] = xs
sortedMerge [] ys = ys
sortedMerge xxs@(x:xs) yys@(y:ys) =
  if x <= y then x : sortedMerge xs yys else y : sortedMerge xxs ys

-- naive inefficient version
tcount' :: Ord a => a -> [(a, b)] -> Int
tcount' z tys = scount z (map fst tys)

-- NOTE: assumes table to be sorted!
tcount :: Ord a => a -> [(a, b)] -> Int
tcount z = length . dropWhile ((z>=) . fst)

prop_SortedMergeCorrect :: Ord a => OrderedList a -> OrderedList a -> Bool
prop_SortedMergeCorrect (Ordered xs) (Ordered ys) =
  sortedMerge xs ys == sort (xs ++ ys)

prop_MSCVersionsEquivalent :: Ord a => NonEmptyList a -> Bool
prop_MSCVersionsEquivalent (NonEmpty xs) = msc' xs == msc xs
