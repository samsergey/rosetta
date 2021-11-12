import Data.List
import Data.Ord
import Data.Maybe
import Data.Numbers.Primes (primeFactors, isPrime)

data Poly a = Poly { coefficients :: ![a] }
  deriving (Eq, Ord)

instance (Ord a, Eq a, Num a, Show a) => Show (Poly a) where
  show (Poly p) =
    let showMono r (c, i) = r ++ case (c, i) of
          (0, _) -> ""
          (c, 0) -> show c
          (1, i) -> " + " ++ "x^" ++ show i
          (-1, i) -> " - " ++ "x^" ++ show i
          (c, i) | c < 0 -> " - " ++ show (-c) ++ "*x^" ++ show i
          (c, i) | c > 0 -> " + " ++ show c ++ "*x^" ++ show i
    in foldl showMono "" $ zip (reverse p) [0..]

trim :: (Eq a, Num a) => Poly a -> Poly a
trim (Poly p) = Poly $ dropWhile (== 0) p

leading :: Num a => Poly a -> a
leading (Poly []) = 0
leading (Poly (a:_)) = a

degree :: Poly a -> Int
degree (Poly []) = 0
degree (Poly p) = length p - 1

instance Real a => Real (Poly a) where
  toRational (Poly p) = toRational $ last p

instance Enum a => Enum (Poly a) where
  toEnum n = Poly [toEnum n]
  fromEnum (Poly p) = fromEnum $ last p

instance (Eq a, Num a) => Num (Poly a) where

  Poly p1 + Poly p2 = trim . Poly $ zipWith (+) (align d p1) (align d p2)
    where
      d = length p1 `max` length p2
      align n p = replicate (n - length p) 0 ++ p
  
  Poly p1 * Poly p2 = sum $ Poly <$> res
    where
      res = zipWith (map . (*)) (reverse p1) $ (p2 ++) <$> inits (repeat 0)

  fromInteger n = Poly [fromInteger n]
  negate (Poly p) = Poly $ negate <$> p
  abs (Poly p) = Poly $ abs <$> p
  signum (Poly p) = Poly $ signum <$> p
  
instance (Real a, Fractional a, Enum a) => Integral (Poly a) where
  toInteger (Poly p) = undefined

  quotRem (Poly p1) (Poly p2) = (Poly q, Poly r)
    where (q, r) = polydiv p1 p2

monomial n x = Poly $ x : replicate n 0

------------------------------------------------------------
 
shift n l = l ++ replicate n 0
 
pad n l = replicate n 0 ++ l
 
norm :: (Eq a, Fractional a) => [a] -> [a]
norm = dropWhile (== 0)
 
deg l = length (norm l) - 1
 
zipWith' op p q = zipWith op (pad (-d) p) (pad d q)
  where d = length p - length q
 
polydiv f g = aux (norm f) (norm g) []
  where aux f s q | ddif < 0 = (q, f)
                  | otherwise = aux f' s q'
           where ddif = deg f - deg s
                 k = head f / head s
                 ks = map (* k) $ shift ddif s
                 q' = zipWith' (+) q $ shift ddif [k]
                 f' = norm $ tail $ zipWith' (-) f ks

polyadd :: Num a => [a] -> [a] -> [a]
polyadd [] ys          = ys
polyadd xs []          = xs
polyadd (x:xs) (y:ys)  = (x+y) : polyadd xs ys

polyAddScalar :: Num a => a -> [a] -> [a]
polyAddScalar c [] = [c]
polyAddScalar c (x:xs) = (c+x):xs

polysub :: Num a => [a] -> [a] -> [a]
polysub [] ys          = map negate ys
polysub xs []          = xs
polysub (x:xs) (y:ys)  = (x-y) : polysub xs ys

-- | Scale a polynomial

polyscale :: Num a => a -> [a] -> [a]
polyscale a x = map (a*) x

-- | Multiply two polynomials

polymult :: Num a => [a] -> [a] -> [a]
polymult ys =
   foldr (\x acc -> polyadd (polyscale x ys) (0 : acc)) []

polydiv'' :: Fractional a => [a] -> [a] -> [a]
polydiv'' x0 y0 = reverse $ polydiv' (reverse x0) (reverse y0)
    where polydiv' (x:xs) y
             | length (x:xs) < length y = []
             | otherwise = z : (polydiv' (tail (polysub (x:xs) (polymult [z] y))) y)
                where z = x / head y
          polydiv' [] _ = []

------------------------------------------------------------

-- Gorner scheme
--subst :: Poly a -> Poly a -> Poly a
subst (Poly p) x = foldl (\r c -> r * x + Poly [c]) (Poly []) p

lift p 1 = p
lift (Poly p) n = Poly $ intercalate (replicate (n-1) 0) (pure <$> p)

isPrime' = isPrime . fromIntegral

-- Cyclotomic polynomials
cyclotomic' :: Int -> Poly Double
cyclotomic' 0 = Poly []
cyclotomic' 1 = Poly [1,-1]
cyclotomic' n
  | isPrime' n = Poly $ replicate n 1
  | even n && odd n' && isPrime' n' = Poly $ take (n-1) $ cycle [1,-1]
  | otherwise = let ps = nub $ primeFactors n
                    cm = foldl (\c p -> lift c p `div` c) (Poly [1,-1]) ps
                in lift cm $ n `div` product ps
  where n' = n `div` 2

-- Cyclotomic polynomials
cyclotomic :: Int -> Poly Double
cyclotomic 0 = Poly []
cyclotomic 1 = Poly [1,-1]
cyclotomic n
  | isPrime' n = Poly $ replicate n 1
  | even n && odd n' && isPrime' n' = Poly $ take (n-1) $ cycle [1,-1]
  | otherwise = let (p, m):ps = primePowerFactors n
                    r = n `div` (p ^ m)
                    cm = cyclotomic r
                in lift (lift cm p `div` cm) (p^(m-1))
  where n' = n `div` 2


--    p, m, r = next(factors(n))
--    poly = cyclotomic(r)
--    return elevate(poly_div(elevate(poly, p), poly), p**(m-1))

primePowerFactors = map (\x-> (head x, length x)) . group . primeFactors

divisors = map product . mapM (\(p, m)-> [p^i | i<-[0..m]]) . primePowerFactors

properDivisors n = filter (< n) $ divisors n

main = do
  mapM_ (print . cyclotomic') [1..30]
  putStrLn $ replicate 40 '-'
  let cs = cyclotomic' <$> [1..]
  let firstIndexOf c = fromJust . elemIndex c $ maximum . coefficients . abs <$> cs
  mapM_ (\i -> putStrLn $ show i ++ " appears in CM(" ++ show (firstIndexOf i) ++ ")") [1..3]

