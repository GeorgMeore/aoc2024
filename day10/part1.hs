import Input

import Data.Array
import Data.Char

uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq (x:xs) = x:uniq [y | y <- xs, y /= x]

walk :: Array (Int, Int) Char -> (Int, Int) -> [(Int, Int)]
walk m x@(i, j)
  | m!x == '9' = [x]
  | otherwise =
      let next = [y | y@(k, l) <- [(i-1, j), (i+1, j), (i, j-1), (i, j+1)],
                  inRange (bounds m) y && ord (m!y) == ord (m!x) + 1]
       in uniq $ concatMap (walk (m//[(x, '.')])) next

main :: IO ()
main = print $ total
  where zeros = [(i, j) | (i, row) <- zip [0..] input, (j, c) <- zip [0..] row, c == '0']
        m  = array ((0, 0), (length input - 1, length (head input) - 1))
                   [((i, j), v) | (i, row) <- zip [0..] input, (j, v) <- zip [0..] row]
        total = sum $ map (length . (walk m)) zeros