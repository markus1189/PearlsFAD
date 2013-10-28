module Saddleback where

import Test.QuickCheck.Modifiers
import Control.Monad (guard)

{- Task:
Today I would like you design a function invert that takes two
arguments: a function f from pairs of natural numbers to natural
numbers, and a natural number z.  The value invert f z is a list of
all pairs (x,y) satisfying f (x,y) = z.  You can assume that f is
strictly increasing in each argument, but nothing else.
-}

type F = (Int -> Int -> Int)

-- bad if evaluation of f is expensive
naiveInvert :: F -> Int -> [(Int,Int)]
naiveInvert f z = do
  x <- [0..z]
  y <- [0..z]
  guard $ f x y == z
  return (x,y)

-- reduce search area, using the hint that f is increasing
boundedInvert :: F -> Int -> [(Int, Int)]
boundedInvert f z = do
  let fZero = f 0 0
  x <- [0..z - fZero ]
  y <- [0..z - x - fZero]
  guard $ f x y == z
  return (x,y)

strictlyIncreasing :: F -> Positive Int -> Positive Int -> Bool
strictlyIncreasing f (Positive x) (Positive y) = f x y >= x && f x y >= y

{-
 (0,z)                     (z,z)
   +------------------------+
   |                        |
   |                        |
   |       (u,v)            |
   |        +---------------+
   |        |               |
   |        |               |
   |        |               |
   |        |               |
   |        |               |
   |        |               |
   +--------+---------------+
 (0,0)                     (z,0)
-}
naiveFind :: (Int,Int) -> F -> Int -> [(Int,Int)]
naiveFind (u,v) f z = [(x,y) | x <- [u..z], y <- [v,v-1..0], f x y == z]

{-
If u > z or v < 0 then clearly `find (u,v) f z = []`. Otherwise, we
carry out a case analysis on the value f(u,v). If f(u,v) < z, then the
rest of column u can be eliminated since f(u,v') < f(u,v) < z for v' <
v. If f(u,v) > z we can similarly eliminate the rest of row
v. Finally, if f(u,v) = z then we can record (u,v) and eliminate the
rest of both column u and row v.
-}
caseAnalaysisFind :: (Int, Int) -> (Int -> Int -> Int) -> Int -> [(Int, Int)]
caseAnalaysisFind (u,v) f z | u > z || v < 0 = []
                            | z' < z = caseAnalaysisFind (u+1,v) f z
                            | z' == z = (u,v) : caseAnalaysisFind (u+1,v-1) f z
                            | z' > z = caseAnalaysisFind (u,v-1) f z
  where z' = f u v
        z' :: Int

invertViaFind :: F -> Int -> [(Int, Int)]
invertViaFind f z = naiveFind (0,z) f z

bsearch :: Ord a => (Int -> a) -> (Int, Int) -> a -> Int
bsearch g (a,b) z | a+1 == b = a
                  | g m <= z = bsearch g (m,b) z
                  | otherwise = bsearch g (a,m) z
  where m = (a+b) `div` 2
