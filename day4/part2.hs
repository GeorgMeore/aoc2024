module Main where

import TestInput

main :: IO()
main = print count
  where (m, n) = (length chars, length (head chars))
        as = [(i, j) | i <- [0..m-1], j <- [0..n-1], chars!!i!!j == 'A']
        points (i, j) = map (zip "MSMS")
                            [[(i+1, j+1), (i-1, j-1), (i+1, j-1), (i-1, j+1)],
                             [(i-1, j-1), (i+1, j+1), (i+1, j-1), (i-1, j+1)],
                             [(i+1, j+1), (i-1, j-1), (i-1, j+1), (i+1, j-1)],
                             [(i-1, j-1), (i+1, j+1), (i-1, j+1), (i+1, j-1)]]
        match (c, (i, j)) = i >= 0 && j >=0 && i < m && j < n && chars!!i!!j == c
        count = sum [sum [fromEnum (all match y) | y <- points a] | a <- as]
