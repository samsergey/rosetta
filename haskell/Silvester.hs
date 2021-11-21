sPermutations :: [a] -> [([a], Int)]
sPermutations = flip zip (cycle [1, -1]) . foldl aux [[]]
  where
    aux items x = do
      (f, item) <- zip (cycle [reverse, id]) items
      f (insertEv x item)
    insertEv x [] = [[x]]
    insertEv x l@(y:ys) = (x : l) : ((y :) <$>) (insertEv x ys)
 
elemPos :: [[a]] -> Int -> Int -> a
elemPos ms i j = (ms !! i) !! j
 
prod :: Num a => ([[a]] -> Int -> Int -> a) -> [[a]] -> [Int] -> a
prod f ms = product . zipWith (f ms) [0 ..]
 
sDet :: Num a
  => ([[a]] -> Int -> Int -> a) -> [[a]] -> [([Int], Int)] -> a
sDet f ms = sum . fmap (\(is, s) -> fromIntegral s * prod f ms is)
 
det :: Num a => [[a]] -> a
det ms =
  sDet elemPos ms . sPermutations $ [0 .. pred . length $ ms]

silvester p1 p2 =
  take (m+n) <$> (take m (iterate (0:) (p1 ++ repeat 0)) ++
                  take n (iterate (0:) (p2 ++ repeat 0)))
  where
    n = length p1 - 1
    m = length p2 - 1

resultant :: Num a => [a] -> [a] -> a
resultant p q = det $ silvester p q

normalized :: (Eq a, Num a) => [a] -> [a]
normalized = dropWhile (== 0)

isZero :: (Eq a, Num a) => [a] -> Bool
isZero = null . normalized


shortDiv :: (Eq a, Fractional a) => [a] -> [a] -> ([a], [a])
shortDiv p1 p2
  | isZero p2 = error "zero divisor"
  | otherwise =
      let go 0 p = normalized p 
          go i (h:t) = (h/a) : go (i-1) (zipWith (+) (map ((h/a) *) ker) t)
      in splitAt k $ go k p1
  where
    k = length p1 - length as
    a:as = normalized p2
    ker = negate <$> (as ++ repeat 0)

pmod p q = snd $ shortDiv p q

pgcd :: (Eq a, Fractional a) => [a] -> [a] -> [a]
pgcd p q
  | isZero q = p
  | length p > length q = pgcd q (p `pmod` q)
  | otherwise = pgcd p (q `pmod` p)
