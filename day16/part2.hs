import Input

import Data.Map as Map (Map, fromList, (!), insert, member)
import Data.Set as Set (Set, fromList, union, empty)

walk :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Map (Int, Int) Int -> Set (Int, Int)
walk start end v costs =
  let go p@(i, j) v@(di, dj) (acc, path) (costs, points)
        | p == end && costs!p == acc = (costs, points `union` (Set.fromList path))
        | p == end && costs!p > acc  = (insert p acc costs, (Set.fromList (end:path)))
        | costs!p < acc-1000 = (costs, points) -- because of crossroads
        | otherwise =
            let mincost = min acc (costs!p)
                [(pl, vl), (pm, vm), (pr, vr)]
                  | di == 0 = [((i-dj, j), (-dj, 0)), ((i, j+dj), (0, dj)), ((i+dj, j), (dj, 0))]
                  | dj == 0 = [((i, j+di), (0, di)), ((i+di, j), (di, 0)), ((i, j-di), (0, -di))]
                (costs',   points')   = go pl vl (acc+1001, p:path) (insert p mincost costs, points)
                (costs'',  points'')  = go pm vm (acc+1, p:path) (costs', points')
                (costs''', points''') = go pr vr (acc+1001, p:path) (costs'', points'')
             in (costs''', points''')
   in snd $ go start v (0, []) (costs, empty)

main :: IO ()
main =
  let indexed = [((i, j), v) | (i, r) <- zip [0..] maze, (j, v) <- zip [0..] r]
      costs = Map.fromList [(p, if v == '#' then minBound else maxBound) | (p, v) <- indexed]
      start = head [p | (p, 'S') <- indexed]
      end   = head [p | (p, 'E') <- indexed]
      points = walk start end (0, 1) costs
   in print $ length points
