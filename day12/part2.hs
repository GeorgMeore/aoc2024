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

uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq (x:xs) = x:uniq [y | y <- xs, y /= x]

popsides :: Region -> Point -> [(Point, Point)]
popsides ps p@(i, j) =
  let t = takeWhile (`elem` ps) [(i+k, j) | k <- [0..]]
      b = takeWhile (`elem` ps) [(i-k, j) | k <- [0..]]
      r = takeWhile (`elem` ps) [(i, j+k) | k <- [0..]]
      l = takeWhile (`elem` ps) [(i, j-k) | k <- [0..]]
      v = any (`elem` ps) [(i+1, j), (i-1, j)]
      h = any (`elem` ps) [(i, j+1), (i, j-1)]
   in (if h then [(last l, last r)] else []) ++ (if v then [(last b, last t)] else [])

select :: (a -> Bool) -> [a] -> [[a]]
select f l =
  let go _ [] acc = reverse (map reverse acc)
      go False (x:xs) ls
        | f x = go True xs ([x]:ls)
        | otherwise = go False xs ls
      go True (x:xs) (l:ls)
        | f x = go True xs ((x:l):ls)
        | otherwise = go False xs (l:ls)
   in go False l []

adjustment :: Array Point Char -> (Point, Point) -> Int
adjustment m (s@(i, j), e@(k, l)) =
  let lu (a, b) = (a - signum (l - j), b + signum (k - i)) -- left/up
      rd (a, b) = (a + signum (l - j), b - signum (k - i)) -- right/down
      l1 = select (\p -> not (cango m p (lu p))) (range (s, e))
      l2 = select (\p -> not (cango m p (rd p))) (range (s, e))
      total l = sum (map (subtract 1 . length) l)
   in total l1 + total l2

price :: Array Point Char -> Region -> Int
price m r =
  let border = [p | p <- r, any (not . cango m p) (neigh p)]
      perimeter = sum [length (filter (not . cango m p) (neigh p)) | p <- border]
      sides = uniq (concatMap (popsides border) border)
      fixup = sum (map (adjustment m) sides)
      area = length r
   in (perimeter-fixup) * area

main :: IO ()
main = print $ sum $ map (price m) (walkall m)
  where (a, b) = (length garden, length (head garden))
        m = listArray ((0, 0), (a-1, b-1)) (concat garden)
