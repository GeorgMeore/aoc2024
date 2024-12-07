import Input

numcat :: Integer -> Integer -> Integer
numcat x y = x * head (filter (>y) (map (10^) [1..])) + y

check :: (Integer, [Integer]) -> Bool
check (target, nums) = go 0 nums
  where go acc [] = acc == target
        go acc (n:ns) =
          acc <= target && (go (acc*n) ns || go (acc+n) ns || go (numcat acc n) ns)

main :: IO ()
main = print $ sum [target | eq@(target, _) <- equations, check eq]
