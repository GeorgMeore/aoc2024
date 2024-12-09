import Input

import Data.Char
import Data.Array

-- The algorithm goes like that: get to the leftmost hole and search for a fitting
-- file starting from the rightmost. If there is one - place it, if there isn't -
-- this hole will never be filled. Why can we do that instead of actually taking
-- the rightmost file and searching for the leftmost free space?
--   If you'll think carefully, you'll see why such a placement is always correct,
-- and that we do all the possible placements, therefore we must end up in the same state.
--   I could probably use that fact to design a more efficient algorithm, that would
-- use a sorted list of files (by size), so that the defragmentation could be done in
-- linear time, buuuuuuuuuuuuut...
defragment :: Array Int (Int, Int) -> [(Int, Int)]
defragment frags = defloop [] frags 0
  where last = snd (bounds frags)
        findfit n frags j
          | j < 0 = -1
          | snd (frags!j) == -1 || fst (frags!j) > n = findfit n frags (j-1)
          | otherwise = j
        defloop acc frags i
          | i > last = reverse acc
          | even i = defloop (frags!i:acc) frags (i+1)
          | otherwise =
            let (n, -1) = frags!i
                j = findfit n frags last
                (m, id) = frags!j
             in if j < i
                then defloop (frags!i:acc) frags (i+1)
                else defloop ((m, id):acc) (frags // [(i, (n-m, -1)), (j, (m, -1))]) i

checksum :: [(Int, Int)] -> Int
checksum blocks = go 0 0 blocks
  where progsum n = n*(n - 1) `div` 2
        go i acc [] = acc
        go i acc ((n, -1):bs) = go (i+n) acc bs
        go i acc ((n, id):bs) = go (i+n) (acc + id*(progsum n + n*i)) bs

main :: IO ()
main = print $ checksum $ defragment $ frags'
  where frags = [((ord c - ord '0'), (if odd i then -1 else i `div` 2)) | (i, c) <- zip [0..] disk]
        frags' = array (0, length frags-1) (zip [0..] frags)
