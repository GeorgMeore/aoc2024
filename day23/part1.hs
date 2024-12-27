import Input

import Data.Set (fromList, member)

uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq (x:xs) = x:uniq (filter (/= x) xs)

subsets :: Int -> [a] -> [[a]]
subsets 1 l = map (:[]) l
subsets _ [] = []
subsets n (x:xs) = map (x:) (subsets (n - 1) xs) ++ subsets n (xs)

main :: IO ()
main =
  let machines = uniq (map fst connections ++ map snd connections)
      triplets = subsets 3 machines
      connset = fromList [(min a b, max a b) | (a, b) <- connections]
      connected [] = True
      connected (m:ms) =
        and [(min m m', max m m') `member` connset | m' <- ms] && connected ms
      parties = filter connected (filter (any ((== 't') . head)) triplets)
   in print (length parties)
