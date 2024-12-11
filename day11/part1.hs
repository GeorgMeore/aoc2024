import Input

log10 :: Int -> Int
log10 n = head $ filter ((> n) . (10 ^)) [1..]

blink :: Int -> [Int]
blink 0 = [1]
blink n =
  let (p, m) = (log10 n, 10 ^ (p `div` 2))
   in if even p then [n `div` m, n `mod` m] else [n*2024]

main :: IO ()
main = print $ length $ iterate (concatMap blink) stones !! 25
