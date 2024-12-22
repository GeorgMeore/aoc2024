import Input

import Data.Char

npmov :: Char -> Char -> [String]
npmov 'A' t
  | t == '0' = ["<"]
  | t `elem` "369" = map ("^" ++) (npmov '3' t)
  | otherwise = map ("^" ++) (npmov '3' t) ++ map ("<^" ++) (npmov '2' t)
npmov f 'A'
  | f == '0' = [">"]
  | f `elem` "369" = map (++ "v") (npmov f '3')
  | otherwise = map (++ "v") (npmov f '3') ++ map (++ "v>") (npmov f '2')
npmov f '0'
  | f `elem` "369" = map (++ "v<") (npmov f '3') ++ map (++ "v") (npmov f '2')
  | otherwise = map (++ "v") (npmov f '2')
npmov '0' t
  | t `elem` "369" = map (">^" ++) (npmov '3' t) ++ map ("^" ++) (npmov '2' t)
  | otherwise = map ("^" ++) (npmov '2' t)
npmov f t =
  let (fn, tn) = (ord f - ord '0' - 1, ord t - ord '0' - 1)
      (fromrow, fromcol) = (fn `div` 3, fn `mod` 3)
      (torow, tocol) = (tn `div` 3, tn `mod` 3)
      (drow, dcol) = (torow - fromrow, tocol - fromcol)
      vchar = if drow < 0 then 'v' else '^'
      hchar = if dcol < 0 then '<' else '>'
      perms v1 c1 _ 0 = [replicate c1 v1]
      perms _ 0 v2 c2 = [replicate c2 v2]
      perms v1 c1 v2 c2 =
        map (v1:) (perms v1 (c1-1) v2 c2) ++
        map (v2:) (perms v1 c1 v2 (c2-1))
   in perms vchar (abs drow) hchar (abs dcol)

kpmov :: Char -> Char -> [String]
kpmov f t
  | f == t = [""]
  | otherwise =
      let table = [(('^', 'A'), [">"]), (('^', 'v'), ["v"]),  (('^', '<'), ["v<"]),       (('^', '>'), [">v", "v>"]),
                   (('A', '>'), ["v"]), (('A', '^'), ["<"]),  (('A', 'v'), ["v<", "<v"]), (('A', '<'), ["<v<", "v<<"]),
                   (('>', 'A'), ["^"]), (('>', 'v'), ["<"]),  (('>', '<'), ["<<"]),       (('>', '^'), ["^<", "<^"]),
                   (('v', '^'), ["^"]), (('v', '>'), [">"]),  (('v', '<'), ["<"]),        (('v', 'A'), [">^", "^>"]),
                   (('<', 'v'), [">"]), (('<', '>'), [">>"]), (('<', '^'), [">^"]),       (('<', 'A'), [">>^", ">^>"])]
          Just moves = lookup (f, t) table
       in moves

expand :: (Char -> Char -> [String]) -> String -> [String]
expand f s =
  let go curr "" = [""]
      go curr (next:rest) = [mov ++ "A" ++ movs | mov <- f curr next, movs <- go next rest]
   in go 'A' s

(!!!) :: Eq a => [(a, b)] -> a -> b
l !!! k = let Just v = lookup k l in v

--
-- To understand how this works we need to took at how a sequence
-- of button presses is transformed through a sequence of keypads.
--
-- ...^>...^>...         Suppose we have an initial sequence like that,
--    .'    '-.          where we have two (or more) ^> subsequences (in general any other).
--    v       v
-- ...--a--...--b--...   After transformations there will be two images a and b for those
--                       subsequences, that implement "move from button < to button ^ and press".
--
-- If we want to find the shortest possible transformed sequence, then a and b must be of the same
-- length and there must be no shorter sequence c that implements initial sequence (otherwise the
-- sequence obviously wouldn't be shortest). For each two-element sequence we want to find those
-- minimal lenght that we'll call costs.
--
-- Suppose we know costs for a sequence of n - 1 keypads. Lets reason abot the same sequence ^>.
-- By n-th keypad it can be turned either to ">vA" or "v>A". For ">vA" we have 3 subsequences
-- "A>" (we'll always have to move to the first button from A), ">v", "vA", we can sum up the
-- known costs of those subsequences and get the total cost of ">vA" variant.
-- From all of the variants we choose the one with the smallest cost.
--
kpcosts :: Int -> [((Char, Char), Int)]
kpcosts n =
  let go acc 0 = acc
      go acc n =
        go [((prev, curr), minimum [sum [acc!!!pc | pc <- zip ("A" ++ mov) (mov ++ "A")] | mov <- kpmov prev curr])
            | prev <- "<^>vA", curr <- "<^>vA"]
           (n - 1)
   in go [((prev, curr), 1) | prev <- "<^>vA", curr <- "<^>vA"] n

minseq :: String -> Int
minseq code =
  let s1 = expand npmov code
      costs = kpcosts 25
   in minimum [sum [costs!!!pc | pc <- zip ("A" ++ mov) mov] | mov <- s1]

codenum :: String -> Int
codenum code =
  let go "A" acc = acc
      go (d:rest) acc = go rest (acc*10 + ord d - ord '0')
   in go code 0

main :: IO ()
main = print $ sum [codenum code * minseq code | code <- codes]
