import Control.Monad

normalized :: (Eq a, Num a) => [a] -> [a]
normalized = dropWhile (== 0)

isZero :: (Eq a, Num a) => [a] -> Bool
isZero = null . normalized

shortDiv :: (Eq a, Fractional a) => [a] -> [a] -> ([a], [a])
shortDiv p1 p2
  | isZero p2 = error "zero divisor"
  | otherwise = foldM step p1 [1 .. length p1 - length as]
  where
    a:as = normalized p2
    step (h:t) = return ([h/a], zipWith (+) (map ((h/a) *) ker) t)
    ker = negate <$> (as ++ repeat 0)


isMonic :: (Eq a, Num a) => [a] -> Bool
isMonic = ([1] ==) . take 1 . normalized

shortDivMonic :: (Eq a, Num a) => [a] -> [a] -> ([a], [a])
shortDivMonic p1 p2
  | isZero p2 = error "zero divisor"
  | not (isMonic p2) = error "divisor is not monic"
  | otherwise = foldM step p1 [1 .. length p1 - length as]
    where
      a:as = normalized p2
      step (h:t) = return ([h], zipWith (+) (map (h *) ker) t)
      ker = negate <$> as ++ repeat 0

