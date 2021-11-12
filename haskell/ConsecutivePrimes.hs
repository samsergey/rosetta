
primes = 2 : sieve [3,5..]
  where sieve (x:xs) = x : sieve (filter (\y -> y `mod` x /= 0) xs)



newtype List a = List (Int, [a])
  deriving (Show)

push x (List (n, l)) = List $ (n+1, x:l)

instance Eq (List a) where
  l1 == l2 = len l1 == len l2
  
instance Ord (List a) where
  l1 `compare` l2 = len l1 `compare` len l2

single x = List (1, [x])
len (List (n, _)) = n
first (List (_, x:_)) = x
  
-- lcs (Longest Consecutive Sequence)
--lcs :: (a -> a -> Bool) -> [a] -> (Int, [a])
lcs ord (h:t) = foldr f (single h, single h) t
  where
    f y (best, buffer)
      | first buffer `ord` y  = (best, push y buffer)
      | otherwise             = (best `max` buffer, single y)

-- lcs (Longest Consecutive Sequence)
--lcs' :: (a -> a -> Bool) -> [a] -> (Int, [a])
lcs' ord (h:t) = foldl f ((1, [h]), (1, [h])) $ Just <$> t
  where
    f ((bestN, best), (n, x:xs)) y = case y of
      Nothing | n > bestN -> ((n, x:xs), (n, x:xs))
              | otherwise -> ((bestN, best), (n, x:xs))              
      Just y  | x `ord` y   -> ((bestN, best), (n+1, y:x:xs))
              | n > bestN -> ((n    , x:xs), (1  , [y]))
              | otherwise -> ((bestN, best), (1  , [y]))

------------------------------------------------------------
-- generates sequence of runs defined by equivalence relation
conseq ord = filter ((> 1) . length) . go []
  where
    go r [] = [r]
    go [] (h:t) = go [h] t
    go (y:ys) (h:t) | y `ord` h = go (h:y:ys) t
                    | otherwise = (y:ys) : go [h] t

maximumBy g (h:t) = foldr f h t
  where f r x = if g r < g x then x else r

task ord n = reverse $ (p+s) : p : (fst <$> rest)
  where
    (p,s):rest = maximumBy length $ conseq step ps
    step (_,a) (_,b) = b `ord` a
    ps = (\p -> zip p (diff p)) $ takeWhile (< n) primesW
    diff lst = zipWith (-) (tail lst) lst

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
