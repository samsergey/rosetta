-- {-# language DeriveFoldable #-}
import Data.List
import Control.Monad
import Text.Printf

sod :: Integer -> Integer
sod = sum . digits

selfF :: Integer -> Integer
selfF n = n + sod n

digits :: Integer -> [Integer]
digits = unfoldr f
  where f 0 = Nothing
        f n = let (q,d) = divMod n 10 in Just (d,q)

isSelf :: Integer -> Bool
isSelf n | n < 10 = odd n 
isSelf n = all (\i -> selfF (n - c - 9*i) /= n) [0..genericLength ds]
  where c = let d = 1 + (sum ds - 1) `mod` 9
            in if even d then d `div` 2 else (d + 9) `div` 2
        ds = digits n

selfs' = filter isSelf [1..]

------------------------------------------------------------

selfs :: [Integer]
selfs = sieve (sumFs [0..]) [0..]
  where
--    sumFs' = zipWith (+) $ sum <$> replicateM  10 [0..9]
    sumFs = zipWith (+) [ a+b+c+d+e+f+g+h+i+j
                        | a <- [0..9] , b <- [0..9]
                        , c <- [0..9] , d <- [0..9]
                        , e <- [0..9] , f <- [0..9]
                        , g <- [0..9] , h <- [0..9]
                        , i <- [0..9] , j <- [0..9] ] 
    sieve (f:fs) (n:ns)
      | n > f = sieve fs (n:ns)
      | n `notElem` take 81 (f:fs) = n : sieve (f:fs) ns
      | otherwise = sieve (f:fs) ns

------------------------------------------------------------

-- selfs'' :: [Integer]
-- selfs'' = sieve (fromList f) fs [0..]
--   where
--     (f, fs) = splitAt 81 sumFs
--     sumFs = zipWith (+) [0..] [ a+b+c+d+e+f+g+h+i+j
--                               | a <- [0..9] , b <- [0..9]
--                               , c <- [0..9] , d <- [0..9]
--                               , e <- [0..9] , f <- [0..9]
--                               , g <- [0..9] , h <- [0..9]
--                               , i <- [0..9] , j <- [0..9] ]
--     sieve q (f:fs) (n:ns)
--       | n > top q = sieve (enq f (deq q)) fs (n:ns)
--       | n `notElem` q = n : sieve q (f:fs) ns
--       | otherwise = sieve q (f:fs) ns

-- data Q a = Q ![a] ![a]
--   deriving (Show, Foldable)

-- top (Q [] l) = top $ Q (reverse l) []
-- top (Q (x:_) _) = x

-- enq x (Q l r) = Q l (x:r)

-- deq (Q [] r) = deq $ Q (reverse r) []
-- deq (Q (x:l) r) = Q l r

-- fromList lst = Q (lst) []


main = do
  print $ take 50 selfs
  forM_ [1..7] $ \i ->
    printf "1e%v\t%v\n" (i :: Int) (selfs !! (10^i-1))
