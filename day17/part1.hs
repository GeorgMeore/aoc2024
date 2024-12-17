import Input

import Data.Bits

main :: IO ()
main =
  let n = length program
      step a b c [] out = out
      step a b c pc out =
        let (i:l:pc') = pc
            v | l < 4 = l
              | l == 4 = a
              | l == 5 = b
              | l == 6 = c
            out'
              | i == 0 = step (a `div` 2^v) b c pc' out
              | i == 1 = step a (b `xor` l) c pc' out
              | i == 2 = step a (v `mod` 8) c pc' out
              | i == 3 = if a == 0
                         then step a b c pc' out
                         else step a b c (drop l program) out
              | i == 4 = step a (b `xor` c) c pc' out
              | i == 5 = step a b c pc' (v `mod` 8:out)
              | i == 6 = step a (a `div` 2^v) c pc' out
              | i == 7 = step a b (a `div` 2^v) pc' out
         in out'
   in print $ reverse $ step a b c program []
