import Input

import Data.Char
import Data.Array

defragment :: Array Int (Int, Int) -> [(Int, Int)]
defragment frags = go [] frags (bounds frags)
  where go acc frags (i, j)
          | i > j = reverse acc
          | even i = go (frags!i:acc) frags (i+1, j)
          | odd j  = go acc frags (i, j-1)
          | otherwise =
            let ((n, -1), (m, id)) = (frags!i, frags!j)
             in if n > m then
                  go ((m, id):acc) (frags//[(i, (n-m, -1))]) (i, j-2)
                else if m > n then
                  go ((n, id):acc) (frags//[(j, (m-n, id))]) (i+1, j)
                else
                  go ((n, id):acc) frags (i+1, j-2)

checksum :: [(Int, Int)] -> Int
checksum blocks = go 0 0 blocks
  where progsum n = n*(n - 1) `div` 2
        go i acc [] = acc
        go i acc ((n, id):bs) = go (i+n) (acc + id*(progsum n + n*i)) bs

main :: IO ()
main = print $ checksum $ defragment $ frags'
  where frags = [((ord c - ord '0'), (if odd i then -1 else i `div` 2)) | (i, c) <- zip [0..] disk]
        frags' = array (0, length frags-1) (zip [0..] frags)
