import Control.Monad

partDiffDiff n = if odd n then (n + 1) `div` 2 else n + 1

partDiff n = if n < 2 then 1 else partDiff (n - 1) + partDiffDiff (n-1)

partitionsP n
  | n < 2 = 1
  | otherwise = sum [ partitionsP (n - pd) * if ((i-1) `mod` 4) < 2 then 1 else (-1)
                    | i <- [1..n]
                    , let pd = partDiff i, pd  <= n ]


partitionsP' = partitionsP'' <$> [0..]
partitionsP'' n
  | n < 2 = 1
  | otherwise = sum $ zipWith (*) signs ((partitionsP' !!) <$> terms)
  where
    terms = (n -) <$> takeWhile (<= n) (tail ofsets)
    signs = cycle [1,1,-1,-1]

ofsets = scanl (+) 0 [if even n then n `div` 2 else n | n <- [1..]]



partitions :: [Integer]
partitions = 1 : 1 : rest
 where
   rest = [] -- (scanl (\r (o, t) -> scanl (+) 0 (zipWith o r t)) (repeat 0) (zip signs terms))
   terms = map (`drop` partitions) ofsets
   signs = cycle [1,1,-1,-1]

main = print $ partitionsP'' 6666
