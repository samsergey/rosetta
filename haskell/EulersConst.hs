divergensy :: Double -> [Double] -> (Int, Double)
divergensy eps lst = (length series, snd (last series))
  where
    dist (x1,x2) = abs (x1 - x2)
    series = takeWhile ((> eps) . dist) $ zip lst (tail lst)

harmonics = scanl (+) 0 $ recip <$> [1..]


directEstimation = zipWith (-) harmonics (log <$> [1..])

