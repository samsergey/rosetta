partDiffDiff n = if odd n then (n + 1) `div` 2 else n + 1

partDiff n = if n < 2 then 1 else partDiff (n - 1) + partDiffDiff (n-1)

partitionsP n
  | n < 2 = 1
  | otherwise = sum [ partitionsP (n - pd) * if ((i-1) `mod` 4) < 2 then 1 else (-1)
                    | i <- [1..n]
                    , let pd = partDiff i, pd  <= n ]


data Tree a = Node a (Tree a) (Tree a)

Node a tl tr !!! 0 = a 
Node a tl tr !!! n =
   if odd n
     then tl !!! top
     else tr !!! (top-1)
        where top = n `div` 2

instance Functor Tree where
   fmap f (Node a tl tr) = Node (f a) (fmap f tl) (fmap f tr)

--naturals = Node 0  (fmap ((+1).(*2)) naturals) (fmap ((*2).(+1)) naturals)

-- naturals r n =
--    Node n
--      ((naturals $! r2) $! (n+r))
--      ((naturals $! r2) $! (n+r2))
--         where r2 = 2*r

partitionsP' = partitionsP'' <$> naturals
partitionsP'' n
  | n < 2 = 1
  | otherwise = sum $ zipWith (*) signs ((partitionsP' !!!) <$> terms)
  where
    terms = (n -) <$> takeWhile (<= n) (tail ofsets)
    signs = cycle [1,1,-1,-1]

ofsets = scanl (+) 0 [if even n then n `div` 2 else n | n <- [1..]]


main = print $ partitionsP'' 6666
