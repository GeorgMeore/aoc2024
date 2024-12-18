import Input

bfs :: (Int, Int) -> (Int, Int) -> [(Int, Int)] -> Bool
bfs start end bad =
  let go done [] = end `elem` done
      go done (p@(i, j):todo) =
        let neigh = [p' | p'@(i', j') <- [(i+1, j), (i-1, j), (i, j+1), (i, j-1)],
                          i' >= 0 && j' >= 0 && i' <= size && j' <= size,
                          p' `notElem` done && p' `notElem` todo]
         in go (p:done) (todo ++ neigh)
   in go bad [start]

bisect :: (Int -> Bool) -> Int -> Int -> Int
bisect f l r
  | l < r =
      let m = l + (r - l) `div` 2
       in if f m
          then bisect f (m + 1) r
          else bisect f l m
  | otherwise = r

main :: IO ()
main =
  let check n = bfs (0, 0) (size, size) (take n falling)
      n = bisect check 0 (length falling)
   in print $ falling!!(n - 1)
