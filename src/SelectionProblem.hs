module SelectionProblem where

import Control.Applicative
import Data.List (nub)
import Test.QuickCheck

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

-- X and Y are two DISJOINT sets of elements
-- (size X + size Y) > k
-- compute: k-th smallest element of X `union` Y

-- k-th smallest: exactly k elements smaller than it, 0-th smallest is
-- smallest
smallest :: Ord a => [a] -> [a] -> Int -> Either String a
smallest xs ys k = union xs ys <&> (!!k)

-- merge two DISJOINT and INCREASING lists
union :: Ord a => [a] -> [a] -> Either String [a]
union xs [] = Right xs
union [] ys = Right ys
union (x:xs) (y:ys) | x < y = (x:) <$> union xs (y:ys)
                    | x > y = (y:) <$> union (x:xs) ys
                    | otherwise = Left "Assumption failure: disjoint and increasing lists"

{-
property of !!:
(xs ++ ys) !! k = if k < |xs| then xs!!k else ys!!(k-|xs|)
-}
prop_divideAndConcquerBangBang :: Eq a => NonEmptyList a -> NonEmptyList a -> Property
prop_divideAndConcquerBangBang (NonEmpty xs) (NonEmpty ys) = do
  let maxLength = length $ xs ++ ys
      k = choose (0,maxLength-1)
  forAll k $ \k' ->
      (xs ++ ys) !! k' == if k' < length xs then xs !! k' else ys !! (k' - length xs)

{-
suppose xs ++ ys and us ++ vs are two sorted disjoint lists such that:
xs `union` vs = xs ++ vs and union us `union` ys = us ++ ys

in other words: no element of xs is >= to any element of vs, (analog for us,ys)
-}

(\\\) :: Eq a => [a] -> [a] -> [a]
xs \\\ ys = filter (`notElem` ys) xs

prop_abidingOperators :: Ord a => OrderedList a -> OrderedList a -> Bool
prop_abidingOperators (Ordered as) (Ordered bs) = do
  let (xs,vs) = nubbedSplit as
      (us,ys) = nubbedSplit $ bs \\\ as -- as and bs have to be disjoint
  (xs ++ ys) `union` (us ++ vs) == ((++) <$> (xs `union` us) <*> (ys `union` vs))
    where splitMiddle :: [a] -> ([a],[a])
          splitMiddle xs = splitAt (length xs `div` 2) xs

          nubbedSplit :: (Eq a, Ord a) => [a] -> ([a],[a])
          nubbedSplit = splitMiddle . nub


-- abbreviated property like in the book
(<<?) :: Ord a => [a] -> [a] -> Bool
xs <<? ys = xs `union` ys == Right (xs ++ ys)
