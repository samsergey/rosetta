-- generates consecutive subsequences defined by given
-- equivalence relation
consecutives equiv = filter ((> 1) . length) . go []
  where
    go r [] = [r]
    go [] (h:t) = go [h] t
    go (y:ys) (h:t) | y `equiv` h = go (h:y:ys) t
                    | otherwise = (y:ys) : go [h] t

-- finds maximal values in a list and returns the first one
maximumBy g (h:t) = foldr f h t
  where f r x = if g r < g x then x else r

-- the task implementation
task ord n = reverse $ (p+s) : p : (fst <$> rest)
  where
    (p,s):rest = maximumBy length
               $ consecutives (\(_,a) (_,b) -> a `ord` b)
               $ differences
               $ takeWhile (< n) primesW
    differences l = zip l $ zipWith (-) (tail l) l

------------------------------------------------------------

primesW :: [Int]   
primesW = [2,3,5,7] ++ _Y ( (11:) . gapsW 13 (tail wheel) . _U .
                            map (\p->  
                              map (p*) . dropWhile (< p) $
                                scanl (+) (p - rem (p-11) 210) wheel) )
  where
    _Y g = g (_Y g)
    _U ((x:xs):t) = x : (merge xs . _U . pairs) t
    pairs (xs:ys:t) = merge xs ys : pairs t
    merge xs@(x:xs') ys@(y:ys') | x < y     = x : merge xs' ys
                                | y < x     = y : merge xs  ys'
                                | otherwise = x : merge xs' ys'
 
gapsW k (d:w) s@(c:cs) | k < c     = k : gapsW (k+d) w s    -- set difference
                       | otherwise =     gapsW (k+d) w cs   --   k==c
 
wheel = 2:4:2:4:6:2:6:4:2:4:6:6:2:6:4:2:6:4:6:8:4:2:4:2:    -- gaps = (`gapsW` cycle [2])
        4:8:6:4:6:2:4:6:2:6:6:4:2:4:6:2:6:4:2:4:2:10:2:10:wheel

main = pure ()
