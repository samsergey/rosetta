{-# LANGUAGE DeriveFunctor #-}
import Data.Numbers.Primes (primes)
import Data.Bits

------------------------------------------------------------
-- memoization utilities

data Memo a = Node a (Memo a) (Memo a)
  deriving Functor

memo :: Integral a => Memo p -> a -> p
memo (Node a l r) n
  | n == 0 = a
  | odd n = memo l (n `div` 2)
  | otherwise = memo r (n `div` 2 - 1)

nats :: Integral a => Memo a
nats = Node 0 ((+1).(*2) <$> nats) ((*2).(+1) <$> nats)

memoize :: Integral a => (a -> b) -> a -> b
memoize f = memo (f <$> nats)

memoize2 :: (Integral a, Integral b) => (a -> b -> c) -> a -> b -> c
memoize2 f = memoize (memoize . f)

memoList :: [b] -> Integer -> b
memoList = memo . mkList
  where
    mkList (x:xs) = Node x (mkList l) (mkList r)
      where (l,r) = split xs
            split [] = ([],[])
            split [x] = ([x],[])
            split (x:y:xs) = let (l,r) = split xs in (x:l, y:r)

------------------------------------------------------------

isqrt :: Integer -> Integer
isqrt n = go n 0 (q `shiftR` 2)
 where
   q = head $ dropWhile (< n) $ iterate (`shiftL` 2) 1
   go z r 0 = r
   go z r q = let t = z - r - q
              in if t >= 0
                 then go t (r `shiftR` 1 + q) (q `shiftR` 2)
                 else go z (r `shiftR` 1) (q `shiftR` 2)

p :: Integer -> Integer
p = memoList (undefined : primes)

phi = memoize2 phiM
  where
    phiM x 0 = x
    phiM x a = phi x (a-1) - phi (x `div` p a) (a - 1)

legendrePi :: Integer -> Integer
legendrePi n
  | n < 2 = 0
  | otherwise = phi n a + a - 1
    where a = legendrePi (floor (sqrt (fromInteger n)))

main = mapM_ (\n -> putStrLn $ show n ++ "\t" ++ show (legendrePi (10^n))) [1..7]
