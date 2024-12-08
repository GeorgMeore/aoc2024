import Input

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x:xs) = [(x, y) | y <- xs] ++ pairs xs

uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq (x:xs) = x:uniq [y | y <- xs, y /= x]

main :: IO ()
main = print count
  where (m, n) = (length input, length (head input))
        antennas = [[(i, j) | (i, r) <- zip [0..] input, (j, y) <- zip [0..] r, y == x]
                    | x <- ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']]
        as = [a | l <- antennas,
                  ((i1, j1), (i2, j2)) <- pairs l,
                  a@(k, l) <- [(i2*2-i1, j2*2-j1), (i1*2-i2, j1*2-j2)],
                  k >= 0 && k < m && l >= 0 && l < n]
        count = length $ uniq as
