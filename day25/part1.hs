import Input

count :: (a -> Bool) -> [a] -> Int
count f l = length (filter f l)

transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose l = (map head l):transpose (map tail l)

fits :: [Int] -> [Int] -> Bool
fits key lock = all (<= 7) (zipWith (+) lock key)

main :: IO ()
main =
  let locks = [map (count (== '#')) (transpose s) | s <- schematics, s!!0!!0 == '#']
      keys = [map (count (== '#')) (transpose s) | s <- schematics, s!!0!!0 == '.']
   in print $ sum [count (fits k) locks | k <- keys]
