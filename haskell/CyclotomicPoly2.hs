{-# language LambdaCase #-}
{-# language DeriveFunctor #-}

import Data.List
import Data.Numbers.Primes (primeFactors)

negateVar p = zipWith (*) p $ reverse $ take (length p) $ cycle [1,-1]

lift p 1 = p
lift p n = intercalate (replicate (n-1) 0) (pure <$> p)

shortDiv :: [Integer] -> [Integer] -> [Integer]
shortDiv p1 (_:p2) = unfoldr go (length p1 - length p2, p1)
  where
    go (0, _) = Nothing
    go (i, h:t) = Just (h, (i-1, zipWith (+) (map (h *) ker) t))
    ker = negate <$> p2 ++ repeat 0

primePowerFactors = sortOn fst . map (\x-> (head x, length x)) . group . primeFactors

------------------------------------------------------------
-- memoization utilities

data Memo a = Node a (Memo a) (Memo a)
  deriving Functor

memo :: Integral a => Memo p -> a -> p
memo (Node a l r) n
  | n == 0 = a
  | odd n = memo l (n `div` 2)
  | otherwise = memo r (n `div` 2 - 1)

nats :: Memo Int
nats = Node 0 ((+1).(*2) <$> nats) ((*2).(+1) <$> nats)

memoize f = memo (f <$> nats)

------------------------------------------------------------
-- simple memoization
cyclotomics = memoize cyclotomic

cyclotomic :: Int -> [Integer]
cyclotomic 0 = [0]
cyclotomic 1 = [1, -1]
cyclotomic 2 = [1, 1]
cyclotomic n = case primePowerFactors n of
  -- for n = 2^k
  [(2,h)]       -> 1 : replicate (2 ^ (h-1) - 1) 0 ++ [1]
  -- for prime n
  [(p,1)]       -> replicate n 1
  -- for power of prime n
  [(p,m)]       -> lift (cyclotomics p) (p^(m-1))
  -- for n = 2*p and prime p
  [(2,1),(p,1)] -> take (n `div` 2) $ cycle [1,-1]
  -- for n = 2*m and odd m
  (2,1):_       -> negateVar $ cyclotomics (n `div` 2)
  -- general case
  (p, m):ps     -> let cm = cyclotomics (n `div` (p ^ m))
                   in lift (lift cm p `shortDiv` cm) (p^(m-1))

showPoly [] = "0"
showPoly p = foldl1 (\r -> (r ++) . term) $
             dropWhile null $
             foldMap (\(c, n) -> [show c ++ expt n]) $
             zip (reverse p) [0..]
  where
    expt = \case 0 -> ""
                 1 -> "*x"
                 n -> "*x^" ++ show n

    term = \case [] -> ""
                 '0':'*':t -> ""
                 '-':'1':'*':t -> " - " ++ t
                 '1':'*':t -> " + " ++ t
                 '-':t -> " - " ++ t
                 t -> " + " ++ t                   
main = do
  mapM_ (print . showPoly . cyclotomic) [1..30]
  putStrLn $ replicate 40 '-'
  
  mapM_ showLine $ take 9 task2
  where
    showLine (j, i, l) = putStrLn $ concat [ show j
                                            , " appears in CM(", show i
                                            , ") of length ", show l ]
    
    -- in order to make computations faster we leave onle each 5-th polynomial
    task2 = (1,1,2) : tail (search 1 $ zip [0,5..] $ cyclotomic <$> [0,5..])
      where
        search i ((k, p):ps) = if i `notElem` (abs <$> p)
                               then search i ps
                               else (i, k, length p) : search (i+1) ((k, p):ps)

skipBy n [] = []
skipBy n lst = let (x:_, b) = splitAt n lst
               in x:skipBy n b
