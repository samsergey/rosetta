{-# language BangPatterns #-}
import Data.List (unfoldr)
import Data.Ratio
import Data.Tree (Tree (..), levels, unfoldTree)
import Control.Monad.Zip (mzip)

------------------------------------------------------------
-- tree utilities

mkTree :: (a -> a -> a) -> (a, a) -> Tree a
mkTree f =
  unfoldTree $ \(!a, !b) -> let m = f a b in (m, [(a,m), (m,b)])

pathBy :: Ord b => (a -> b) -> Tree a -> b -> [Either a a]
pathBy f (Node a [l,r]) x =
  case x `compare` f a of
    LT -> Left a : pathBy f l x
    EQ -> [Right a]
    GT -> Right a : pathBy f r x 

lookupTree :: Ord a => Tree (a, c) -> a -> c
lookupTree t =
  snd . either id id . last . pathBy fst t

oddFunc f 0 = 0
oddFunc f x = signum x * f (abs x)

--------------------------------------------------------------------------------

toRatio (a, b) = a % b
mediant (!a,!b) (!c,!d) = (a + c, b + d)
mean (!a,!b) (!c,!d) = (a*d + c*b, 2*b*d)
hmean (!a,!b) (!c,!d) = (2*a*c, b*c + a*d)

farey = toRatio <$> mkTree mediant ((0, 1), (1, 1))

diadic = toRatio <$> mkTree mean ((0, 1), (1, 1))

--------------------------------------------------------------------------------

fromFraction (i, f) = fromIntegral i + f

minkowskiQR :: Rational -> Rational
minkowskiQR = fromFraction . fmap transform . properFraction
  where
    transform = oddFunc $ lookupTree (mzip farey diadic)

invMinkowskiQR :: Rational -> Rational
invMinkowskiQR = fromFraction . fmap transform . properFraction
  where
    transform = oddFunc $ lookupTree (mzip diadic farey)

{-
invMinkowskiQR . minkowskiQR
fromFraction . fmap transform' . properFraction . fromFraction . fmap transform . properFraction
fromFraction . fmap transform' . fmap transform . properFraction
fromFraction . fmap (transform' . transform) . properFraction
fromFraction . fmap (lookupTree (minkowski `mzip` farey) . lookupTree (farey `mzip` minkowski)) . properFraction
fromFraction . fmap (lookupTree (minkowski `mzip` farey `mzip` farey `mzip` minkowski) . properFraction
fromFraction . fmap (lookupTree (minkowski `mzip` minkowski) . properFraction
fromFraction . fmap id . properFraction
fromFraction . properFraction
id
-}


--------------------------------------------------------------------------------
-- ?(x) and inverse implemented via diadic numerals

fromDiadic :: (Int, [Int]) -> Double
fromDiadic = fromFraction . fmap (foldr go 0)
  where
    go x r = (fromIntegral x + r)/2

toDiadic :: Double -> (Int, [Int])
toDiadic = fmap (unfoldr go) . properFraction
  where
    go x = case properFraction (x * 2) of
             (0, 0) -> Nothing
             (i, f) -> Just (i `mod` 2, f)

{-
toDiadic . fromDiadic = id

fmap (unfoldr go') . properFraction . fromFraction . fmap (foldr go 0) $ (i,f)
fmap (unfoldr go') . id . fmap (foldr go 0) $ (i,f)
fmap (unfoldr go') . fmap (foldr go 0) $ (i,f)
fmap (unfoldr go' . foldr go 0) $ (i,f)
(i, unfoldr go' . foldr go 0 $ f)
(i, unfoldr go' . foldr go 0 $ (h:t))
(i, unfoldr go' . foldr (\x r -> (x + r)/2) 0 $ (h:t))
(i, unfoldr go' $ (h + foldr (\x r -> (x + r)/2) 0 t) / 2)
(i, unfoldr go' $ (\r -> (h + r) / 2) . foldr (\x r -> (x + r)/2) 0 t)
(i, unfoldr (go' . \r -> (h + r) / 2) $ foldr (\x r -> (x + r)/2) 0 t)
(i, unfoldr (go'' . properFraction . (* 2) . \r -> (h + r) / 2) $ foldr (\x r -> (x + r)/2) 0 t)
(i, unfoldr (go'' . properFraction . (* 2) . (/ 2) . (h +)) $ foldr (\x r -> (x + r)/2) 0 t)
(i, unfoldr (go'' . properFraction . (h +)) $ foldr (\x r -> (x + r)/2) 0 t)
(i, unfoldr (go'' . first (h+) . properFraction) $ foldr (\x r -> (x + r)/2) 0 t)
(i, unfoldr (\(i', f) -> Just ((h + i') `mod` 2, f) . properFraction) $ foldr (\x r -> (x + r)/2) 0 t)
(i, (((h + i') `mod` 2) :) . unfoldr go' . foldr (\x r -> (x + r)/2) 0 $ t)
-}

track :: Ord a => Tree a -> a -> [Int]
track t = fmap (either (const 0) (const 1)) . pathBy id t

follow :: Tree a -> [Int] -> a
follow t = rootLabel . foldl (\t x -> subForest t !! x) t . init

{-
follow t (track t x) == id

rootLabel . foldl ((!!) . subForest) t . init . fmap (either (const 0) (const 1)) . pathBy id t $ x
rootLabel . foldl ((!!) . subForest) (Node a [l,r]) . init . fmap (either (const 0) (const 1)) . pathBy id (Node a [l,r]) $ x
LT rootLabel . foldl ((!!) . subForest) (Node a [l,r]) . init . fmap (either (const 0) (const 1)) . (Left a :) . pathBy id l $ x
LT rootLabel . foldl ((!!) . subForest) (Node a [l,r]) . init . (0 :) . fmap (either (const 0) (const 1)) . pathBy id l $ x
LT rootLabel . foldl ((!!) . subForest) (Node a [l,r]) . (0 :) . init . fmap (either (const 0) (const 1)) . pathBy id l x
LT rootLabel . foldl ((!!) . subForest) l . init . fmap (either (const 0) (const 1)) . pathBy id l x

EQ rootLabel . foldl ((!!) . subForest) (Node a [l,r]) . init . fmap (either (const 0) (const 1)) . pathBy id (Node a [l,r]) $ a
EQ rootLabel . foldl ((!!) . subForest) (Node a [l,r]) . init . fmap (either (const 0) (const 1)) $ [Right a]
EQ rootLabel . foldl ((!!) . subForest) (Node a [l,r]) . init $ [1]
EQ rootLabel . foldl ((!!) . subForest) (Node a [l,r]) $ []
EQ rootLabel $ Node a [l,r]
EQ a

pathBy f (Node a [l,r]) x =
  case x `compare` f a of
    LT -> Left a : pathBy f l x   
    EQ -> [Right a]               
    GT -> Right a : pathBy f r x  

foldl f x0 [] = x0
foldl f x0 (h:t) = foldl f (f x0 h) t

-}

minkowskiQF :: Double -> Double
minkowskiQF = oddFunc $ fromDiadic . fmap transform . properFraction
  where
    transform 0 = []
    transform f = track (fromRational <$> farey) f

invMinkowskiQF :: Double -> Double
invMinkowskiQF = oddFunc $ fromFraction . fmap transform . toDiadic
  where
    transform [] = 0
    transform f = follow (fromRational <$> farey) f 

{-
invMinkowskiQF . minkowskiQF
fromFraction . fmap transform' . toDiadic . fromDiadic . fmap transform . properFraction
fromFraction . fmap transform' . fmap transform . properFraction
fromFraction . fmap (transform' . transform) . properFraction
fromFraction . fmap (follow (fromRational <$> farey) . track (fromRational <$> farey)) . properFraction
fromFraction . fmap (follow t . track t) . properFraction
fromFraction . fmap id . properFraction
fromFraction . properFraction
id
-}

    
--------------------------------------------------------------------------------
-- tests

-- sequence of all positive rationals
rationals = concat (levels sternBrocot)
  where
    sternBrocot = toRatio <$> mkTree mediant (0, 1) (1, 0)

testEq f g  = all (\x -> f x == g x)
testEqF f g = all (\x -> abs (f x - g x) < 1e-11)

testIds :: [[Rational] -> Bool]
testIds = 
  [ testEq  (invMinkowskiQR . minkowskiQR) id
  , testEq  (minkowskiQR . invMinkowskiQR) id . fmap minkowskiQR
  , testEqF (invMinkowskiQF . minkowskiQF) id . fmap fromRational
  , testEqF (minkowskiQF . invMinkowskiQF) id . fmap fromRational
  , testEq  (minkowskiQF . fromRational) (fromRational . minkowskiQR) ]


--------------------------------------------------------------------------------


-- /* Minkowski's question-mark function */
-- double minkowski(double x) {
--     long p = x;
--     if ((double)p > x) --p; /* p=floor(x) */
--     long q = 1, r = p + 1, s = 1, m, n;
--     double d = 1, y = p;
--     if (x < (double)p || (p < 0) ^ (r <= 0))
--         return x; /* out of range ?(x) =~ x */
--     for (;;) { /* invariants: q * r - p * s == 1 && (double)p / q <= x && x < (double)r / s */
--         d /= 2;
--         if (y + d == y)
--             break; /* reached max possible precision */
--         m = p + r;
--         if ((m < 0) ^ (p < 0))
--             break; /* sum overflowed */
--         n = q + s;
--         if (n < 0)
--             break; /* sum overflowed */

--         if (x < (double)m / n) {
--             r = m;
--             s = n;
--         } else {
--             y += d;
--             p = m;
--             q = n;
--         }
--     }
--     return y + d; /* final round-off */
-- }

a ==> b = lookupTree (mzip a b)

mirror t = Node 0 [reflect $ negate <$> t, t]

reflect (Node a [l,r]) = Node a [reflect r, reflect l]

fromRatio n = (numerator n, denominator n)

mean' a b = toRatio $ fromRatio a `mean` fromRatio b
mediant' a b = toRatio $ fromRatio a `mediant` fromRatio b
hmean' a b = toRatio $ fromRatio a `hmean` fromRatio b

--mean' a b = (a + b) / 2

sternBrocot = toRatio <$> mkTree mediant ((0,1), (1,0))

diadic' = mirror $ toRatio <$> mkTree mean ((0,1), (1,0))
  where
    mean (a,b) (1,0) = (a+1,b)
    mean (a,b) (c,d) = (a*d + c*b, 2*b*d)
