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

minseq :: String -> Int
minseq code =
  let s1 = expand npmov code
      s2 = concatMap (expand kpmov) s1
      s3 = concatMap (expand kpmov) s2
   in minimum (map length s3)

codenum :: String -> Int
codenum code =
  let go "A" acc = acc
      go (d:rest) acc = go rest (acc*10 + ord d - ord '0')
   in go code 0

main :: IO ()
main = print $ sum [codenum code * minseq code | code <- codes]
