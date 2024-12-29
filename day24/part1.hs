import Input

import Data.Map (fromList, toList, member, (!), insert)

doop :: String -> Int -> Int -> Int
doop "AND" x y = x * y
doop "OR" x y = if x + y > 0 then 1 else 0
doop "XOR" x y = (x + y) `mod` 2

settle :: [(String, Int)] -> [(String, String, String, String)] -> [(String, Int)]
settle vals conns =
  let go vals [] [] = vals
      go vals [] skip = go vals skip []
      go vals (conn@(s1, op, s2, t):conns) skip
        | s1 `member` vals && s2 `member` vals
          = go (insert t (doop op (vals!s1) (vals!s2)) vals) conns skip
        | otherwise
          = go vals conns (conn:skip)
   in toList (go (fromList vals) conns [])

main :: IO ()
main = print $ sum $ zipWith (*) [v | ('z':_, v) <- settle inputs connections] (map (2^) [0..])
