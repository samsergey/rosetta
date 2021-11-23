limit :: Double -> [Double] -> Maybe (Int, Double)
limit eps lst = case span ((> eps) . dist) pairs  of
                (_,[]) -> Nothing
                (s, _) -> Just (length s, snd (last s)) 
  where
    dist (x1,x2) = abs (x1 - x2)
    pairs = take 10000 $ zip lst (tail lst)

sums = scanl (+) 0
products = scanl (*) 1
a *. lst = map (* a) lst
(.*.) = zipWith (*)
(.-.) = zipWith (-)

harmonics = recip <$> [1..]

harmonicNumbers = sums harmonics

directEstimation = limit 1e-8 $ harmonicNumbers .-. (log <$> [1..])

sweeneyEstimation :: [Double]
sweeneyEstimation = ss
  where
    n = 21
    ss = sums $ rs .*. harmonics
    rs = products $ n *. harmonics
