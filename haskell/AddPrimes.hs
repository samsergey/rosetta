import Data.List (unfoldr)

primes = 2 : sieve [3,5..]
  where sieve (x:xs) = x : sieve (filter (\y -> y `mod` x /= 0) xs)

isPrime n = all (\p -> n `mod` p /= 0) $ takeWhile (< sqrtN) primes
  where sqrtN = round . sqrt . fromIntegral $ n

digits = unfoldr f
  where f 0 = Nothing
        f n = let (q, r) = divMod n 10 in Just (r,q)

isAdditivePrime n = isPrime n && (isPrime . sum . digits) n

