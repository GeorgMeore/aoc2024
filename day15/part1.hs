import Input

import Data.Map (Map, fromList, (!), union, insert, keys)

move :: (Map (Int, Int) Char, (Int, Int)) -> Char -> (Map (Int, Int) Char, (Int, Int))
move (m, p@(i, j)) d =
  let (di, dj) = case d of '>' -> (0, 1)
                           '<' -> (0, -1)
                           '^' -> (-1, 0)
                           'v' -> (1, 0)
      os = takeWhile ((== 'O') . (m !)) [(i+di*c, j+dj*c) | c <- [1..]]
      (il, jl) = last (p:os)
   in if m!(il+di, jl+dj) == '#'
      then (m, p)
      else if null os
           then (m, (i+di, j+dj))
           else (insert (il+di, jl+dj) 'O' (insert (head os) '.' m), (i+di, j+dj))

main :: IO ()
main =
  let w = [((i, j), v) | (i, r) <- zip [0..] warehouse, (j, v) <- zip [0..] r]
      p = head [(i, j) | ((i, j), v) <- w, v == '@']
      m = insert p '.' (fromList w)
      (m', p') = foldl move (m, p) moves
      csum = sum [100*i + j | (i, j) <- keys m', m'!(i, j) == 'O']
   in print csum
