import Input

import Data.Bits

-- By analyzing the code of the program I found out that
-- it essentially does something like that:
--    while (a) {
--        out some_expression(a)
--        a = a div 8
--    }
-- That means that we can start searching for an appropriate number from the "tail" of the program,
-- adding one byte at a time.

main :: IO ()
main =
  let search [] = [0]
      search (i:is) =
        [a | c <- search is,
             c' <- filter (/= 5) [0..7],
             a <- [c*8 + c'],
             ((a `div` 2^(c' `xor` 5)) `xor` (c' `xor` 3)) `mod` 8 == i]
   in print $ head $ search program
