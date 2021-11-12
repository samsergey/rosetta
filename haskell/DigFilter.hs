import Data.List (inits, tails)

-- a = [1.00000000, -2.77555756e-16, 3.33333333e-01, -1.85037171e-17]
-- b = [0.16666667, 0.5, 0.5, 0.16666667]
-- s = [-0.917843918645, 0.141984778794, 1.20536903482, 0.190286794412,
--       -0.662370894973, -1.00700480494, -0.404707073677, 0.800482325044,
--       0.743500089861, 1.01090520172, 0.741527555207, 0.277841675195,
--       0.400833448236, -0.2085993586, -0.172842103641, -0.134316096293,
--       0.0259303398477, 0.490105989562, 0.549391221511, 0.9047198589]

-- convolution of a list by givel kernel
conv :: Num a => [a] -> [a] -> [a]
conv ker = map (dot (reverse ker)) . tails  . pad
  where
    pad v = replicate (length ker - 1) 0 ++ v
    dot v = sum . zipWith (*) v

-- The difital filter
dFilter :: [Double] -> [Double] -> [Double] -> [Double]
dFilter (a0:a) b s = tail res
  where
    res = (/ a0) <$> 0 : zipWith (-) (conv b s) (conv a res)

