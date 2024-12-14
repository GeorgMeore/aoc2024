import Input

after :: Int -> [(Int, Int)]
after s = [((i+di*s) `mod` n, (j+dj*s) `mod` m) | ((i, j), (di, dj)) <- robots]

draw :: [(Int, Int)] -> IO ()
draw r = putStr s
  where s = concat [[if (i, j) `elem` r then '.' else ' ' | i <- [0..n]] ++ "\n" | j <- [0..m]]

-- Use this to find the frequences and starting points
search :: Int -> IO ()
search n =
  do print n
     draw (after n)
     stop <- getLine
     if stop == "y"
       then print n
       else search (n + 1)

main :: IO ()
main =
  do let n = head [i | i <- [2,105..], (i-23) `mod` 101 == 0]
     print n
     draw (after n)
