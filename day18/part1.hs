import Input

dijkstra :: [(Int, Int)] -> (Int, Int) -> (Int, Int) -> Int
dijkstra bad start end =
  let go costs done =
        let (cost, p@(i, j)) = minimum costs
            neigh = [(cost + 1, p') | p'@(i', j') <- [(i+1, j), (i-1, j), (i, j+1), (i, j-1)],
                                      i' >= 0 && j' >= 0 && i' <= size && j' <= size,
                                      p' `notElem` done && p' `notElem` bad]
         in if p == end
            then cost
            else go (neigh ++ filter ((/= p) . snd) costs) (p:done)
   in go [(0, start)] []

main :: IO ()
main = print $ dijkstra (take 12 falling) (0, 0) (size, size)
