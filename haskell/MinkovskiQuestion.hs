import Data.List (unfoldr)
import Data.Ratio
import Data.Tree (Tree (..), levels, unfoldTree)
import Control.Monad.Zip (mzip)

------------------------------------------------------------
-- tree utilities

mkTree :: (a -> a -> a) -> a -> a -> Tree a
mkTree f a b = unfoldTree go (a, b)
  where
    go (a,b) = let m = f a b in (m, [(a,m), (m,b)])

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
mediant (a,b) (c,d) = (a + c, b + d)
mean (a,b) (c,d) = (a*d + c*b, 2*b*d)

farey = toRatio <$> mkTree mediant (0, 1) (1, 1)

sternBrocot = toRatio <$> mkTree mediant (0, 1) (1, 0)

minkowski = toRatio <$> mkTree mean (0, 1) (1, 1)

--------------------------------------------------------------------------------

fromFraction (i, f) = fromIntegral i + f

minkowskiQR :: Ratio Integer -> Ratio Integer
minkowskiQR = fromFraction . fmap transform . properFraction
  where
    transform = oddFunc $ lookupTree (mzip farey minkowski)

invMinkowskiQR :: Ratio Integer -> Ratio Integer
invMinkowskiQR = fromFraction . fmap transform . properFraction
  where
    transform = oddFunc $ lookupTree (mzip minkowski farey)

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
fromDiadic = fromFraction . fmap (foldr go 0 . take 55)
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
track t = fmap (either (const 0) (const 1)) . pathBy' id t

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

testIds :: [[Ratio Integer] -> Bool]
testIds = 
  [ testEq  (invMinkowskiQR . minkowskiQR) id
  , testEq  (minkowskiQR . invMinkowskiQR) id . fmap minkowskiQR
  , testEqF (invMinkowskiQF . minkowskiQF) id . fmap fromRational
  , testEqF (minkowskiQF . invMinkowskiQF) id . fmap fromRational
  , testEq  (minkowskiQF . fromRational) (fromRational . minkowskiQR) ]


--------------------------------------------------------------------------------

{-

=={{header|Haskell}}==

=== Exact rational function using Farey tree ===

In a lazy functional language Minkowski question mark function can be implemented using one of it's basic properties: 

?(p+r)/(q+s) = 1/2 * ( ?(p/q) + ?(r/s) ), ?(0) = 0, ?(1) = 1.

where p/q and r/s are fractions, such that |ps - rq| = 1.

This recursive definition can be implemented as lazy corecursion, i.e. by generating two infinite binary trees: '''mediant'''-based Farey tree, containing all rationals, and '''mean'''-based tree with corresponding values of Minkowsky ?-function. There is one-to-one correspondence between these two trees so both {{math|?(x)}} and {{math|?<sup>-1</sup>(x)}} may be implemented as mapping between them.

First we define tools to handle trees.

<lang haskell>import Data.List (unfoldr)
import Data.Ratio
import Data.Tree (Tree (..), levels, unfoldTree)
import Control.Monad.Zip (mzip)

--------------------------------------------------------------------------------
-- some type transformations

fromFraction (i, f) = fromIntegral i + f
toFloat (a, b) = fromIntegral a / fromIntegral b
toRatio (a, b) = a % b
fromRatio r = (numerator r, denominator r)

oddFunc f 0 = 0
oddFunc f x = signum x * f (abs x)

------------------------------------------------------------
-- general tree utilities

mkTree :: (a -> a -> a) -> a -> a -> Tree a
mkTree f a b = unfoldTree go (a, b)
  where
    go (a,b) = let m = f a b in (m, [(a,m), (m,b)])

pathBy :: Ord b => (a -> b) -> Tree a -> b -> [Either a a]
pathBy f (Node a [l,r]) x =
  case x `compare` f a of
    LT -> Left a : pathBy f l x
    EQ -> [Right a]
    GT -> Right a : pathBy f r x</lang>

Now it is possible to define two trees:

<lang haskell>mediant (a,b) (c,d) = (a + c, b + d)
mean (a,b) (c,d) = (a*d + c*b, 2*b*d)

farey = toRatio <$> mkTree mediant (0, 1) (1, 1)

minkowski = toRatio <$> mkTree mean (0, 1) (1, 1)</lang>

<pre>λ> mapM_ print $ take 4 $ levels farey
[1 % 2]
[1 % 3,2 % 3]
[1 % 4,2 % 5,3 % 5,3 % 4]
[1 % 5,2 % 7,3 % 8,3 % 7,4 % 7,5 % 8,5 % 7,4 % 5]

λ> mapM_ print $ take 4 $ levels minkowski
[1 % 2]
[1 % 4,3 % 4]
[1 % 8,3 % 8,5 % 8,7 % 8]
[1 % 16,3 % 16,5 % 16,7 % 16,9 % 16,11 % 16,13 % 16,15 % 16]</pre>

Here is symmetric definitions of {{math|?(x)}} and {{math|?<sup>-1</sup>(x)}} for rational numbers:

<lang haskell>lookupTree :: Ord a => Tree (a, c) -> a -> c
lookupTree t =
  snd . either id id . last . pathBy fst t

minkowskiQR :: Ratio Integer -> Ratio Integer
minkowskiQR = fromFraction . fmap transform . properFraction
  where
    transform = oddFunc $ lookupTree (mzip farey minkowski)

invMinkowskiQR :: Ratio Integer -> Ratio Integer
invMinkowskiQR = fromFraction . fmap transform . properFraction
  where
    transform = oddFunc $ lookupTree (mzip minkowski farey)</lang>

<pre>λ> minkowskiQR (1/2)
1 % 2
λ> minkowskiQR (2/7)
3 % 16
λ> minkowskiQR (-22/7)
(-193) % 64
λ> invMinkowskiQR (3/16)
2 % 7
λ> invMinkowskiQR (13/256)
5 % 27</pre>

=== Floating point function using Farey tree ===

Paths leading to numbers in Farey tree, give diadic representation of corresponding value of Minkowski ?-function and it's argument. So it is possible to use Farey tree to define Minkowski function for floating point numbers.

<lang haskell>fromDiadic :: (Int, [Int]) -> Double
fromDiadic = fromFraction . fmap (foldr go 0 . take 55)
  where
    go x r = (r + fromIntegral x)/2

toDiadic :: Double -> (Int, [Int])
toDiadic = fmap (unfoldr go) . properFraction
  where
    go x = case properFraction (x * 2) of
             (0, 0) -> Nothing
             (i, f) -> Just (i `mod` 2, f)

track :: Ord a => Tree a -> a -> [Int]
track t =
  fmap (either (const 0) (const 1)) . pathBy id t

follow :: Tree a -> [Int] -> a
follow t lst =
  rootLabel $ foldl (\t -> (subForest t !!)) t $ init lst

minkowskiQF :: Double -> Double
minkowskiQF = oddFunc $ fromDiadic . fmap transform . properFraction
  where
    transform 0 = []
    transform f = track (fromRational <$> farey) f

invMinkowskiQF :: Double -> Double
invMinkowskiQF = oddFunc $ fromFraction . fmap transform . toDiadic
  where
    transform [] = 0
    transform f = follow (fromRational <$> farey) f </lang>

<pre>λ> minkowskiQF (1/2)
0.5
λ> minkowskiQF (2/7)
0.1875
λ> minkowskiQF (-22/7)
-3.015625
λ> invMinkowskiQF (3/16)
0.2857142857142857
λ> invMinkowskiQF (13/256)
0.18518518518518517
λ> minkowskiQF (sqrt 2)
1.4000000000003183</pre>

The task and tests:

<lang haskell>-- sequence of all positive rationals
sternBrocot = toRatio <$> mkTree mediant (0, 1) (1, 0)
rationals = concat (levels sternBrocot)

testEq f g  = all (\x -> f x == g x)
testEqF f g = all (\x -> abs (f x - g x) < 1e-11)

testIds :: [[Ratio Integer] -> Bool]
testIds = 
  [ testEq  (invMinkowskiQR . minkowskiQR) id
  , testEq  (minkowskiQR . invMinkowskiQR) id . fmap minkowskiQR
  , testEqF (invMinkowskiQF . minkowskiQF) id . fmap fromRational
  , testEqF (minkowskiQF . invMinkowskiQF) id . fmap fromRational
  , testEq  (minkowskiQF . fromRational) (fromRational . minkowskiQR) ]</lang>

λ> minkowskiQF $ (sqrt 5 + 1) / 2
1.6666666666678793
λ> 5/3
1.6666666666666667
λ> invMinkowskiQF (-5/9)
-0.5657414540893351
λ> (sqrt 13 - 7)/6
-0.5657414540893352
λ> sequence testIds $ take 1000 rationals
[True,True,True,True,True]</pre>

-}

