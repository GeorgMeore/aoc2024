import Input

import Data.Array

type Point = (Int, Int)
type Region = [Point]

neigh :: Point -> [Point]
neigh (i, j) = [(i+1, j), (i-1, j), (i, j+1), (i, j-1)]

cango :: Array Point Char -> Point -> Point -> Bool
cango m from to = inRange (bounds m) to && m!to == m!from

walkregion :: Array Point Char -> (Region, Array Point Bool) -> (Region, Array Point Bool)
walkregion m ([], vis) = ([], vis)
walkregion m (ps@(a:_), vis) =
  let addnext (ps, vis) p
        | cango m a p && not (vis!p) = (p:ps, vis//[(p, True)])
        | otherwise = (ps, vis)
      (ps', vis') = foldl addnext ([], vis) (concatMap neigh ps)
      (ps'', vis'') = walkregion m (ps', vis')
   in (ps ++ ps'', vis'')

walkall :: Array Point Char -> [Region]
walkall m =
  let walknext (rs, vis) p
        | vis!p = (rs, vis)
        | otherwise =
            let (r, vis') = walkregion m ([p], vis//[(p, True)])
             in (r:rs, vis')
      vis = listArray (bounds m) (repeat False)
   in fst $ foldl walknext ([], vis) (indices m)

price :: Array Point Char -> Region -> Int
price m r =
  let perimeter = sum $ [length . filter (not . cango m p) $ neigh p | p <- r]
      area = length r
   in perimeter * area

main :: IO ()
main = print $ sum $ map (price m) (walkall m)
  where (a, b) = (length garden, length (head garden))
        m = listArray ((0, 0), (a-1, b-1)) (concat garden)
