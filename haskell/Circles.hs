import Data.Ratio

circ n = [((n^2-i^2)/(n^2 + i^2), 2*i*n/(n^2 + i^2)) | i <- [1..n-1]]

diameter n = product $ denominator . fst <$> circ n

fact n = product [1..n]

triplets n = [ ((n^2 + i^2) `div` k, (n^2-i^2) `div` k, (2*i*n) `div` k)
             | i <- [1..n-1]
             , let k = if i == 1 && odd n then 2 else (gcd n i)^2 ]

integerPts n =
  let r = fromInteger (diameter n) / 2
  in [ (x, y) | x <- [0..r], y <- [0..2*r], x^2 + (y-r)^2 == r^2 ]
