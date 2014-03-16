module MakingACentury where

import           Data.Foldable (traverse_)
import           Data.List (intercalate)
import qualified Prelude
import           Prelude hiding (unzip, zip)

{-
Given the list of digits [1..9], list all ways the operations (+) and
(*) can be inserted for a total of 100

e.g.: (1 * 2) + 34 + 5 + (6 * 7) + 8 + 9
-}

main :: IO ()
main = do
  let bound = 100
      digits = [1..9]
      sols = solutions bound digits
  traverse_ (putStrLn . prettyPrint) sols

type Expression = [Term]
type Term = [Factor]
type Factor = [Digit]
type Digit = Int

solutions :: Int -> [Digit] -> [Expression]
solutions c = map fst . filter (good c . snd) . foldr (expand c) []

expand :: Int
       -> Digit
       -> [(Expression, (Int, Int, Int, Int))]
       -> [(Expression, (Int, Int, Int, Int))]
expand _ x [] = [([[[x]]],(10,x,1,0))]
expand c x evs = concatMap (filter (ok c . snd) . glue x) evs

valExpr :: Expression -> Int
valExpr = sum . map valTerm

valTerm :: Term -> Int
valTerm = product . map valFact

valFact :: Factor -> Int
valFact = foldl1 (\n d -> 10 * n + d)

value :: Expression -> (Int, Int, Int, Int)
value ((xs:xss):xsss) = (10^n, valFact xs, valTerm xss, valExpr xsss)
  where n = length xs

good :: Int -> (Int, Int, Int, Int) -> Bool
good c (_,f,t,e) = f*t + e == c

ok :: Int -> (Int, Int, Int, Int) -> Bool
ok c (_,f,t,e) = f*t + e <= c

cross :: (a -> c, b -> d) -> (a, b) -> (c, d)
cross (f,g) (x,y) = (f x, g y)

fork :: (a -> b, a -> c) -> a -> (b, c)
fork (f,g) x = (f x, g x)

unzip :: [(a, b)] -> ([a], [b])
unzip = fork (map fst, map snd)

zip :: ([a], [b]) -> [(a, b)]
zip = uncurry Prelude.zip

glue :: Int
     -> (Expression, (Int, Int, Int, Int))
     -> [(Expression, (Int, Int, Int, Int))]
glue x ((xs:xss):xsss,(k,f,t,e)) = [ (((x:xs):xss):xsss, (10*k,k*x+f,t,e))
                                   , (([x]:xs:xss):xsss, (10,x,f*t,e))
                                   , ([[x]]:(xs:xss):xsss, (10,x,1,f*t+e))]

modify :: Int -> (Int, Int, Int, Int) -> [(Int, Int, Int, Int)]
modify x (k,f,t,e) = [(10*k, k*x+f,t,e), (10,x,f*t,e),(10,x,1,f*t+e)]

prettyPrint :: Expression -> String
prettyPrint = intercalate " + " . map prettyPrintTerm

prettyPrintTerm :: Term -> String
prettyPrintTerm = intercalate " * " . map prettyPrintFactor

prettyPrintFactor :: Factor -> String
prettyPrintFactor = concatMap show
