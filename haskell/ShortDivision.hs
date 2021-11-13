import Data.List

normalized :: (Eq a, Num a) => [a] -> [a]
normalized = dropWhile (== 0)

isZero :: (Eq a, Num a) => [a] -> Bool
isZero = null . normalized

shortDiv :: (Eq a, Fractional a) => [a] -> [a] -> ([a], [a])
shortDiv p1 p2
  | isZero p2 = error "zero divisor"
  | otherwise =
      let go 0 p = p 
          go i (h:t) = (h/a) : go (i-1) (zipWith (+) (map ((h/a) *) ker) t)
      in splitAt k $ go k p1
  where
    k = length p1 - length as
    a:as = normalized p2
    ker = negate <$> (as ++ repeat 0)

    


isMonic :: (Eq a, Num a) => [a] -> Bool
isMonic = ([1] ==) . take 1 . normalized

shortDivMonic :: (Eq a, Num a) => [a] -> [a] -> ([a], [a])
shortDivMonic p1 p2
  | isZero p2 = error "zero divisor"
  | not (isMonic p2) = error "divisor is not monic"
  | otherwise =
      let go 0 p = p 
          go i (h:t) = h : go (i-1) (zipWith (+) (map (h *) ker) t)
      in splitAt k $ go k p1
    where
      k = length p1 - length as
      _:as = normalized p2
      ker = negate <$> as ++ repeat 0

      


