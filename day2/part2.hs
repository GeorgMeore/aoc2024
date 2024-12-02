module Main where

import TestInput

safe :: [Int] -> Bool
safe [] = True
safe r =
  let diffs = [y - x | (x, y) <- zip r (tail r)]
   in all (\d -> abs d >= 1 && abs d <= 3) diffs &&
      abs (sum (map signum diffs)) == length r - 1

safe' :: [Int] -> Bool
safe' r = any safe (r:[[x | (j, x) <- zip [0..] r, j /= i] | i <- [0..length r]])

count :: (a -> Bool) -> [a] -> Int
count f l = sum [if f x then 1 else 0 | x <- l]

main :: IO ()
main = print $ count safe' reports
