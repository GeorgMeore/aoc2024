import Input

uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq (x:xs) = x:uniq [y | y <- xs, y /= x]

main :: IO ()
main = print $ sum [p !! div (length p) 2 | p <- pages, check p []]
  where items = uniq [c | (a, b) <- rules, c <- [a, b]]
        prereq = [(x, [a | (a, b) <- rules, b == x]) | x <- items]
        check [] e = True
        check (p:ps) e = notElem p e && check ps (maybe e (++ e) (lookup p prereq))
