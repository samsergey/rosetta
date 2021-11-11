import Data.Ratio

primes = 2 : sieve [3,5..]
  where
    sieve (n:ns) = n : filter (\x -> x `mod` n /= 0) (sieve ns)

(>>>) = flip (.)

------------------------------------------------------------
int2nat :: Integer -> Integer
nat2int :: Integer -> Integer

int2nat n | n == 0 = 0
          | n > 0  = 2*n - 1
          | n < 0  = (-2)*n

nat2int n | n == 0 = 0
          | even n = negate $ n `div` 2
          | odd n  = (n + 1) `div` 2

------------------------------------------------------------

nat2primes :: Integer -> [Integer]
primes2nat :: [Integer] -> Integer

nat2primes = go primes 0
  where
    go _ i 1 = [i]
    go (p:ps) i n = if n `mod` p == 0
                    then go (p:ps) (i+1) (n `div` p)
                    else i : go ps 0 n

primes2nat = product . zipWith (^) primes

------------------------------------------------------------

rat2nat :: Ratio Integer -> Integer
nat2rat :: Integer -> Ratio Integer

rat2nat r = let p1 = nat2primes (numerator r)
                p2 = negate <$> nat2primes (denominator r)
            in primes2nat (int2nat <$> p1) * primes2nat (int2nat <$> p2)

nat2rat = nat2primes >>> map nat2int >>> primes2rat

------------------------------------------------------------

primes2rat :: [Integer] -> Ratio Integer
rat2primes :: Ratio Integer -> [Integer]

primes2rat = product . zipWith (^^) (toRational <$> primes)

rat2primes = rat2nat >>> nat2primes >>> map nat2int

------------------------------------------------------------

rat2contfrac :: Ratio Integer -> [Integer]
rat2contfrac r = if f == 0
                 then [i]
                 else i : rat2contfrac (recip f)
  where i = floor r
        f = r - toRational i

contfrac2rat :: [Integer] -> Ratio Integer
contfrac2rat [] = 0
contfrac2rat [x] = toRational x
contfrac2rat (x:xs) = toRational x + recip (contfrac2rat xs)

------------------------------------------------------------

list2nat :: [Integer] -> Integer
list2nat = contfrac2rat >>> rat2nat

nat2list :: Integer -> [Integer]
nat2list = nat2rat >>> rat2contfrac

------------------------------------------------------------

