{-# LANGUAGE DeriveFunctor #-}
import Data.Numbers.Primes (primes)
import Data.Bits

------------------------------------------------------------
-- memoization utilities

type Memo2 a = Memo (Memo a)

data Memo a = Node a (Memo a) (Memo a)
  deriving Functor

memo :: Integral a => Memo p -> a -> p
memo (Node a l r) n
  | n == 0 = a
  | odd n = memo l (n `div` 2)
  | otherwise = memo r (n `div` 2 - 1)

memo2 :: Integral a => Memo2 b -> a -> a -> b
memo2 f = memo . memo f

nats :: Integral a => Memo a
nats = Node 0 ((+1).(*2) <$> nats) ((*2).(+1) <$> nats)

memoize :: Integral a => (a -> b) -> Memo b
memoize f = f <$> nats

memoize2 :: (Integral a, Integral b) => (a -> b -> c) -> Memo2 c
memoize2 f = memoize (memoize . f)

memoList :: [b] -> Memo b
memoList (x:xs) = Node x (memoList l) (memoList r)
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

p :: Memo Integer
p = memoList (undefined : primes)

phi :: Integer -> Integer -> Integer
phi x 0 = x
phi x a = memo2 phiM x (a-1) - memo2 phiM (x `div` memo p a) (a - 1)

phiM :: Memo2 Integer
phiM = memoize2 phi

legendrePi :: Integer -> Integer
legendrePi n
  | n < 2 = 0
  | otherwise = memo2 phiM n a + a - 1 where a = legendrePi (isqrt n)

main = mapM_ (\n -> putStrLn $ show n ++ "\t" ++ show (legendrePi (10^n))) [1..4]
