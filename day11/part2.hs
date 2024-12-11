import Input

import Data.Map (empty, toList, member, adjust, insert)

log10 :: Int -> Int
log10 n = head $ [i | i <- [1..], 10^i > n]

blink :: [(Int, Int)] -> [(Int, Int)]
blink ss = ss'
  where blink1 0 = [1]
        blink1 n =
          let (p, m) = (log10 n, 10 ^ (p `div` 2))
           in if even p then [n `div` m, n `mod` m] else [n*2024]
        dedup counts [] = toList counts
        dedup counts ((s, c):ss)
          | s `member` counts = dedup (adjust (+ c) s counts) ss
          | otherwise = dedup (insert s c counts) ss
        ss' = dedup empty [(s', c) | (s, c) <- ss, s' <- blink1 s]

main :: IO ()
main = print $ sum $ map snd $ iterate blink [(s, 1) | s <- stones] !! 75
