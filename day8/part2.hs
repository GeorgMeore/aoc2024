import Input

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x:xs) = [(x, y) | y <- xs] ++ pairs xs

uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq (x:xs) = x:uniq [y | y <- xs, y /= x]

antinodes :: ((Int, Int), (Int, Int)) -> (Int, Int) -> [(Int, Int)]
antinodes ((i1, j1), (i2, j2)) (m, n) =
  let (di, dj) = (i2 - i1, j2 - j1)
      g = gcd (abs di) (abs dj) -- The biggest amount of pieces each d? can be split
      (di', dj') = (di `div` g, dj `div` g)
      inbounds (i, j) = i >= 0 && i < m && j >= 0 && j < n
      left = takeWhile inbounds [(i1-di'*c, j1-dj'*c) | c <- [0..]]
      right = takeWhile inbounds [(i1+di'*c, j1+dj'*c) | c <- [1..]]
   in left ++ right

main :: IO ()
main = print count
  where (m, n) = (length input, length (head input))
        antennas = [[(i, j) | (i, r) <- zip [0..] input, (j, y) <- zip [0..] r, y == x]
                    | x <- ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']]
        nodes = [n | a <- antennas, p <- pairs a, n <- antinodes p (m, n)]
        count = length $ uniq nodes
