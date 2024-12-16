import Input

import Data.Map (Map, fromList, (!), union, insert, keys, findMax)

move :: (Map (Int, Int) Char, (Int, Int)) -> Char -> (Map (Int, Int) Char, (Int, Int))
move (m, p@(i, j)) d =
  let (di, dj)
        | d == '>' = (0, 1)
        | d == '<' = (0, -1)
        | d == '^' = (-1, 0)
        | d == 'v' = (1, 0)
      boxes p@(i, j)
        | di == 0 = takeWhile ((`elem` "[]") . (m !)) [(i, j+dj*c) | c <- [0..]]
        | m!p == '[' = [(i, j), (i, j+1)] ++ boxes (i+di, j) ++ boxes (i+di, j+1)
        | m!p == ']' = [(i, j), (i, j-1)] ++ boxes (i+di, j) ++ boxes (i+di, j-1)
        | otherwise = []
      unmovable (i, j) = m!(i+di, j+dj) == '#'
      bs = boxes (i+di, j+dj)
      (m', p')
        | any unmovable (p:bs) = (m, p)
        | otherwise = (diff `union` m, (i+di, j+dj))
      clear = fromList [(b, '.') | b <- bs]
      move  = fromList [((i+di, j+dj), m!b) | b@(i, j) <- bs]
      diff  = move `union` clear
   in (m', p')

main :: IO ()
main =
  let expand '@' = "@."
      expand 'O' = "[]"
      expand c   = [c, c]
      w = [((i, j'), v') | (i, r) <- zip [0..] warehouse,
                           (j, v) <- zip [0..] r,
                           (j', v') <- zip [j*2, j*2+1] (expand v)]
      p = head [(i, j) | ((i, j), v) <- w, v == '@']
      m = insert p '.' (fromList w)
      (m', p') = foldl move (m, p) moves
      csum = sum [100*i + j | (i, j) <- keys m', m'!(i, j) == '[']
   in print csum
