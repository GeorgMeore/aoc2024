import Input

-- na*ax + nb*bx = px
-- na*ay + nb*by = py
price :: ((Int, Int), (Int, Int), (Int, Int)) -> Int
price ((ax, ay), (bx, by), (px, py)) =
  let (nb, rb) = divMod (py*ax - px*ay) (by*ax - bx*ay)
      (na, ra) = divMod (px - nb*bx) ax
   in if ra /= 0 || rb /= 0 then 0 else na*3 + nb

main :: IO ()
main = print $ sum $ [price (a, b, (px + 10000000000000, py + 10000000000000)) | (a, b, (px, py)) <- machines]
