import Input

uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq (x:xs) = x:uniq [y | y <- xs, y /= x]

tsort :: Eq a => [(a, [a])] -> [a] -> [a]
tsort prereq items = go [] prereq'
  where go acc [] = acc
        go acc prereq =
          let avail = [x | (x, []) <- prereq] -- This must be nonempty, otherwise the sequence is not sortable
           in go (acc ++ avail)
                 [(x, filter (`notElem` avail) p) | (x, p) <- prereq, p /= []]
        prereq' = [(x, filter (`elem` items) p) | (x, p) <- prereq, x `elem` items]

main :: IO ()
main = print $ sum [p' !! div (length p') 2 | (p, p') <- zip pages pages', p /= p']
  where items = uniq [c | (a, b) <- rules, c <- [a, b]]
        prereq = [(x, [a | (a, b) <- rules, b == x]) | x <- items]
        pages' = map (tsort prereq) pages
