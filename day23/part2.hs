import Input

import Data.List
import Data.Map as Map (fromList, (!))
import Data.Set as Set (fromList, member)

subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = let ss = subsets xs in map (x:) ss ++ ss

main :: IO ()
main =
  let machines = (map head . group . sort) (map fst connections ++ map snd connections)
      adj = Map.fromList [(m, [if a == m then b else a  | (a, b) <- connections, a == m || b == m]) | m <- machines]
      conn = Set.fromList [(min a b, max a b) | (a, b) <- connections]
      connected [] = True
      connected (m:ms) =
        and [(min m m', max m m') `member` conn | m' <- ms] && connected ms
      maxgroup m = maximum [(length s, (m:s)) | s <- subsets (adj!m), connected (m:s)]
      elems = snd(maximum (map maxgroup machines))
   in print $ sort elems
