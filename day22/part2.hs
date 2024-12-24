import Input

import Data.Bits
import Data.List (zip4)
import Data.Map (Map, fromList, (!), keysSet, member)
import Data.Set (union, empty, toList)

next :: Int -> Int
next n =
  let n'   = ((n * 64) `xor` n) `mod` 16777216
      n''  = ((n' `div` 32) `xor` n') `mod` 16777216
      n''' = ((n'' * 2048) `xor` n'') `mod` 16777216
   in n'''

seqmap :: [Int] -> Map (Int, Int, Int, Int) Int
seqmap prices =
  let changes = zipWith (-) (drop 1 prices) prices
      quads = zip4 changes (drop 1 changes) (drop 2 changes) (drop 3 changes)
   in fromList (reverse (zip quads (drop 4 prices)))

main :: IO ()
main =
  let prices = map (map (`mod` 10) . take 2000 . iterate next) secrets
      seqmaps = map seqmap prices
      allseqs = toList (foldl union empty (map keysSet seqmaps))
      gains = [sum [m!s | m <- seqmaps, s `member` m] | s <- allseqs]
  in print $ maximum gains
