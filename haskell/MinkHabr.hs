import Data.Tree

stern


extendFunc f = fromFraction . fmap (oddFunc f) . properFraction
  where
    oddFunc f 0 = 0
    oddFunc f x = signum x * f (abs x)
    fromFraction (i,f) = fromIntegral i + f
