import Input

-- I just reverse-engineered the building block of the adder and
-- used this procedure to locate bad spots and then fixed them manually.
fixup :: [(String, String, String, String)] -> IO ()
fixup conns =
  let out x op y =
        head ([out | conn@(a, op', b, out) <- conns, op' == op, (a, b) == (x, y) || (a, b) == (y, x)] ++ [""])
      go carry [] = putStrLn "OK"
      go carry (bit:bits) =
        let xsum = out ('x':bit) "XOR" ('y':bit)
            ofl = out ('x':bit) "AND" ('y':bit)
            zout = out carry "XOR" xsum
            cofl = out xsum "AND" carry
            carry' = out ofl "OR" cofl
         in if or [xsum == "", ofl == "", zout /= 'z':bit, cofl == "", carry' == ""]
            then print (bit, carry, xsum, ofl, zout, cofl, carry')
            else go carry' bits
   in go (out "x00" "AND" "y00") (['0':show i | i <- [1..9]] ++ [show i | i <- [10..44]])

main :: IO ()
main = fixup connections
