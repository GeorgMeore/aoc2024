import Input

price :: ((Int, Int), (Int, Int), (Int, Int)) -> Int
price ((ax, ay), (bx, by), (px, py)) =
  let prices = [na*3 + nb | (na, nb) <- [(i, j) | i <- [0..100], j <- [0..100]],
                na*ax+nb*bx == px && na*ay+nb*by == py]
   in if prices == [] then 0 else minimum prices

main :: IO ()
main = print $ sum $ map price machines
