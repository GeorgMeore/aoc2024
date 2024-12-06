import Input

uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq (x:xs) = x:uniq [y | y <- xs, y /= x]

main :: IO ()
main = print $ length $ uniq $ walk start (-1, 0)
  where (m, n) = (length labmap, length (head labmap))
        [start] = [(i, j) | i <- [0..m-1], j <- [0..n-1], labmap!!i!!j == '^']
        obst = [(i, j) | i <- [0..m-1], j <- [0..n-1], labmap!!i!!j == '#']
        turn (x, 0) = (0, -x)
        turn (0, x) = (x, 0)
        walk (i, j) (di, dj)
          | i < 0 || j < 0 || i >= m || j >= n = []
          | (i, j) `elem` obst = walk (i-di, j-dj) (turn (di, dj))
          | otherwise = (i, j):walk (i+di, j+dj) (di, dj)
