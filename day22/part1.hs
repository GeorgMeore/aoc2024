import Input

import Data.Bits

next :: Int -> Int
next n =
  let n'   = ((n * 64) `xor` n) `mod` 16777216
      n''  = ((n' `div` 32) `xor` n') `mod` 16777216
      n''' = ((n'' * 2048) `xor` n'') `mod` 16777216
   in n'''

main :: IO ()
main = print $ sum [(iterate next s) !! 2000 | s <- secrets]
