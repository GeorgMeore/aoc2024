module Main where

import TestInput

main :: IO()
main = print count
  where (m, n) = (length chars, length (head chars))
        xs = [(i, j) | i <- [0..m-1], j <- [0..n-1], chars!!i!!j == 'X']
        points (i, j) = map (zip "XMAS")
                            [[(i+k, j)   | k <- [0..3]], [(i-k, j)   | k <- [0..3]],
                             [(i, j+k)   | k <- [0..3]], [(i, j-k)   | k <- [0..3]],
                             [(i+k, j+k) | k <- [0..3]], [(i+k, j-k) | k <- [0..3]],
                             [(i-k, j+k) | k <- [0..3]], [(i-k, j-k) | k <- [0..3]]]
        match (c, (i, j)) = i >= 0 && j >=0 && i < m && j < n && chars!!i!!j == c
        count = sum [sum [fromEnum (all match y) | y <- points x] | x <- xs]
