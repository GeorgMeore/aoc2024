import Input

main :: IO ()
main = print factor
  where steps = 100
        after = [((i+di*steps) `mod` n, (j+dj*steps) `mod` m) | ((i, j), (di, dj)) <- robots]
        lu = [p | p@(i, j) <- after, i < n `div` 2 && j < m `div` 2]
        ru = [p | p@(i, j) <- after, i > n `div` 2 && j < m `div` 2]
        lb = [p | p@(i, j) <- after, i < n `div` 2 && j > m `div` 2]
        rb = [p | p@(i, j) <- after, i > n `div` 2 && j > m `div` 2]
        factor = product (map length [lu, ru, lb, rb])
