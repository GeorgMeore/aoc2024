module Main where

import TestInput

safe :: [Int] -> Bool
safe [] = True
safe r =
  let ds = [y - x | (x, y) <- zip r (tail r)]
   in all (\d -> abs d >= 1 && abs d <= 3) ds &&
      abs (sum (map signum ds)) == length r - 1

count :: (a -> Bool) -> [a] -> Int
count f l = sum [if f x then 1 else 0 | x <- l]

main :: IO ()
main = print $ count safe reports
