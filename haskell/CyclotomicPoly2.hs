import Data.List
import Data.Maybe
import Data.Numbers.Primes (primeFactors)

showPoly p = foldl showMono "" $ zip (reverse p) [0..]
  where
    showMono r (c, i) = r ++ case (c, i) of
      (0, _) -> ""
      (c, 0) -> show c
      (1, i) -> " + " ++ "x^" ++ show i
      (-1, i) -> " - " ++ "x^" ++ show i
      (c, i) | c < 0 -> " - " ++ show (-c) ++ "*x^" ++ show i
      (c, i) | c > 0 -> " + " ++ show c ++ "*x^" ++ show i 

------------------------------------------------------------

negateVar p = zipWith (*) p ones
  where ones = if odd (length p) then cycle [1,-1] else cycle [-1,1]

lift p 1 = p
lift p n = intercalate (replicate (n-1) 0) (pure <$> p)

shortDiv :: [Integer] -> [Integer] -> [Integer]
shortDiv p1 (_:p2) = unfoldr go (length p1 - length p2, p1)
  where
    go (0, _) = Nothing
    go (i, h:t) = Just (h, (i-1, zipWith (+) (map (h *) ker) t))
    ker = negate <$> p2 ++ repeat 0

primePowerFactors = sortOn fst . map (\x-> (head x, length x)) . group . primeFactors
    

cyclotomics :: [[Integer]]
cyclotomics = [1,-1] : (cyclotomic <$> [1..])

cyclotomic :: Int -> [Integer]
cyclotomic 0 = []
cyclotomic 1 = [1,-1]
cyclotomic 2 = [1,1]
cyclotomic n = case primePowerFactors n of
  [(2,h)]       -> 1 : replicate (2 ^ (h-1) - 1) 0 ++ [1]
  [(p,1)]       -> replicate n 1
  [(p,m)]       -> lift (cyclotomics !! p) (p^(m-1))
  [(2,1),(p,1)] -> take (n `div` 2) $ cycle [1,-1]
  (2,1):_       -> negateVar $ cyclotomics !! (n `div` 2)
  (p, m):ps     -> let cm = cyclotomics !! (n `div` (p ^ m))
                   in lift (lift cm p `shortDiv` cm) (p^(m-1))


main = do
  mapM_ (print . showPoly . cyclotomic) [1..30]
  putStrLn $ replicate 40 '-'
  let indexes = take 4 $ ixs $ maximum . map abs <$> cyclotomics
  mapM_ (\(j, i) -> putStrLn $ show j ++ " appears in CM(" ++ show i ++ ")") $
    zip [1..] indexes

ixs :: [Integer] -> [Int]
ixs l = tail $ scanl (+) 0 $ unfoldr f (1, l)
  where
    f (i, l) = let (a, b) = span (/= i) l in Just (length a, (i+1, b))
