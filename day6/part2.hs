import Input

import Data.HashSet

rturn :: (Int, Int) -> (Int, Int)
rturn (x, 0) = (0, -x)
rturn (0, x) = (x, 0)

walk :: (Int, Int) -> HashSet (Int, Int) -> (Int, Int) -> Int
walk (m, n) obst start = go start (-1, 0) empty empty obst False
  where go p@(i, j) v@(di, dj) states visited obst put
          | i+di < 0 || j+dj < 0 || i+di >= m || j+dj >= n = 0
          | member (i+di, j+dj) obst =
              go p (rturn v) states visited obst put
          | put && member (p, v) states = 1
          | not put && not (member (i+di, j+dj) visited) =
              go (i+di, j+dj) v (insert (p, v) states) (insert p visited) obst False +
              go p v states visited (insert (i+di, j+dj) obst) True
          | otherwise = go (i+di, j+dj) v (insert (p, v) states) (insert p visited) obst put


main :: IO ()
main = print $ walk (m, n) obst start
  where (m, n) = (length labmap, length (head labmap))
        [start] = [(i, j) | i <- [0..m-1], j <- [0..n-1], labmap!!i!!j == '^']
        obst = fromList [(i, j) | i <- [0..m-1], j <- [0..n-1], labmap!!i!!j == '#']
