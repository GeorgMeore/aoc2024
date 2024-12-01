module Main where

import TestInput

while :: (a -> Bool) -> (a -> a) -> a -> a
while c f x
  | c x = while c f (f x)
  | otherwise = x

merge :: Ord a => [a] -> [a] -> [a]
merge x [] = x
merge [] y = y
merge x@(a:x') y@(b:y')
  | b < a     = b:merge x y'
  | otherwise = a:merge x' y

sort :: Ord a => [a] -> [a]
sort [] = []
sort l =
  let compact (l1:l2:ls) = (merge l1 l2:compact ls)
      compact l = l
   in head $ while ((> 1) . length)
                   compact
                   [[x] | x <- l]

main :: IO ()
main =
  let l1 = sort $ map fst locations
      l2 = sort $ map snd locations
   in print $ sum [abs (x - y) | (x, y) <- zip l1 l2]
