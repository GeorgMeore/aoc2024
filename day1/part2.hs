module Main where

import Debug.Trace

import TestInput

while :: (a -> Bool) -> (a -> a) -> a -> a
while c f x
  | c x = while c f (f x)
  | otherwise = x

cmerge :: Ord a => [(a, Int)] -> [(a, Int)] -> [(a, Int)]
cmerge x [] = x
cmerge [] y = y
cmerge x@((a, ac):x') y@((b, bc):y')
  | b == a    = cmerge ((a, ac+bc):x') y'
  | b < a     = (b, bc):cmerge x y'
  | otherwise = (a, ac):cmerge x' y

csort :: Ord a => [a] -> [(a, Int)]
csort [] = []
csort l =
  let compact (l1:l2:ls) = (cmerge l1 l2:compact ls)
      compact l = l
   in head $ while ((> 1) . length)
                   compact
                   [[(x, 1)] | x <- l]

main :: IO ()
main =
  let score [] _ = 0
      score _ [] = 0
      score l1@((a, ac):l1') l2@((b, bc):l2')
        | a == b = ac*bc*a + score l1' l2'
        | a < b  = score l1' l2
        | a > b  = score l1 l2'
      l1 = csort $ map fst locations
      l2 = csort $ map snd locations
   in print $ score l1 l2
