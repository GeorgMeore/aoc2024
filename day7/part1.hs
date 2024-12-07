import Input

check :: (Integer, [Integer]) -> Bool
check (target, nums) = go 0 nums
  where go acc [] = acc == target
        go acc (n:ns) = (acc*n <= target && go (acc*n) ns) ||
                        (acc+n <= target && go (acc+n) ns)

main :: IO ()
main = print $ sum [target | eq@(target, _) <- equations, check eq]
