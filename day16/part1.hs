import Input

import Data.Map

walk :: (Int, Int) -> (Int, Int) -> Int -> Map (Int, Int) Int -> Map (Int, Int) Int
walk p@(i, j) v@(di, dj) acc costs
  | costs!p < acc-1000 = costs -- because of crossroads
  | otherwise =
      let mincost = min acc (costs!p)
          [(pl, vl), (pm, vm), (pr, vr)]
            | di == 0 = [((i-dj, j), (-dj, 0)), ((i, j+dj), (0, dj)), ((i+dj, j), (dj, 0))]
            | dj == 0 = [((i, j+di), (0, di)), ((i+di, j), (di, 0)), ((i, j-di), (0, -di))]
          costs'   = walk pl vl (acc+1001) (insert p mincost costs)
          costs''  = walk pm vm (acc+1) costs'
          costs''' = walk pr vr (acc+1001) costs''
       in costs'''

main :: IO ()
main =
  let indexed = [((i, j), v) | (i, r) <- zip [0..] maze, (j, v) <- zip [0..] r]
      costs = fromList [(p, if v == '#' then minBound else maxBound) | (p, v) <- indexed]
      start = head [p | (p, 'S') <- indexed]
      end   = head [p | (p, 'E') <- indexed]
      costs' = walk start (0, 1) 0 costs
   in print (costs'!end)
