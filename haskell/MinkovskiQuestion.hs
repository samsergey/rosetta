{-# language DeriveFunctor #-}
import Data.List (unfoldr)
import Data.Ratio

--------------------------------------------------------------------------------
-- infinite binary tree

data Tree a = Node (Tree a) a (Tree a)
  deriving Functor

node (Node _ a _ ) = a

-- smart constructor
mkTree :: (a -> a -> a) -> a -> a -> Tree a
mkTree f l r = Node (mkTree f l m) m (mkTree f m r)
  where m = f l r

-- searching a number in the tree
track :: (Num a, Ord a) => Tree a -> a -> [Int]
track t x = unfoldr go t ++ [1]
  where
    go (Node l a r) = case compare x a of
        LT -> Just (0, l)
        EQ -> Nothing
        GT -> Just (1, r)

-- following the path in the tree
follow :: Tree a -> [Int] -> a
follow t lst =  node $ foldl go t $ init lst
  where
    go (Node l a r) x = [l, r] !! x

-- list of levels of the tree
levels :: Tree a -> [[a]]
levels (Node l a r) = [a] : zipWith (++) (levels l) (levels r)

--------------------------------------------------------------------------------
-- some type transformations

toFraction :: RealFrac a => a -> (Int, a)
toFraction = properFraction

fromFraction :: RealFrac a => (Int, a) -> a
fromFraction (i, f) = fromIntegral i + f

toFloat :: Fractional a =>  (Int, Int) -> a
toFloat (a, b) = fromIntegral a / fromIntegral b

toRatio (a, b) = a % b
fromRatio r = (numerator r, denominator r)

oddFunc f 0 = 0
oddFunc f x = signum x * f (abs x)

--------------------------------------------------------------------------------
-- enumeration of all rationals and their mapping with Minkowski ? function

mean (a,b) (c,d) = (a*d + c*b, 2*b*d)
mediant (a,b) (c,d) = (a + c, b + d)

farey :: Tree (Ratio Integer)  
farey = toRatio <$> mkTree mediant (0, 1) (1, 1)
   
minkowski :: Tree (Ratio Integer)  
minkowski = toRatio <$> mkTree mean (0, 1) (1, 1)
    
--------------------------------------------------------------------------------
-- ?(x) and inverse implemented as mapping between trees

minkowskiQR :: Ratio Integer -> Ratio Integer
minkowskiQR = fromFraction . fmap transform . toFraction
  where
    transform = oddFunc (follow minkowski . track farey)

invMinkowskiQR :: Ratio Integer -> Ratio Integer
invMinkowskiQR = fromFraction . fmap transform . toFraction
  where
    transform = oddFunc (follow farey . track minkowski)

--------------------------------------------------------------------------------
-- ?(x) and inverse implemented via diadic numerals

fromDiadic :: (Int, [Int]) -> Double
fromDiadic = fromFraction . fmap (foldr go 0 . take 55)
  where
    go x r = (r + fromIntegral x)/2

toDiadic :: Double -> (Int, [Int])
toDiadic = fmap (unfoldr go) . toFraction
  where
    go x = case properFraction (x * 2) of
             (0, 0) -> Nothing
             (i, f) -> Just (i `mod` 2, f)

minkowskiQF :: Double -> Double
minkowskiQF = oddFunc $ fromDiadic . fmap transform . toFraction
  where
    transform 0 = []
    transform f = track (fromRational <$> farey) f

invMinkowskiQF :: Double -> Double
invMinkowskiQF = oddFunc $ fromFraction . fmap transform . toDiadic
  where
    transform [] = 0
    transform f = follow (fromRational <$> farey) f 
   
--------------------------------------------------------------------------------
-- tests

-- sequence of all positive rationals
rationals = take 1000 $ concat (levels sternBrocot)
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

{-

=={{header|Haskell}}==

=== Using Farey tree ===

In a lazy functional language Minkowski question mark function can be implemented using one of it's basic properties: 

?(p+r)/(q+s) = 1/2 * ( ?(p/q) + ?(r/s) ), ?(0) = 0, ?(1) = 1.

where p/q and r/s are fractions, such that |ps - rq| = 1.

This recursive definition can be implemented as lazy corecursion, i.e. by generating two infinite binary trees: '''mediant'''-based Farey tree, containing all rationals, and '''mean'''-based tree with corresponding values of Minkowsky ?-function. There is one-to-one correspondence between these two trees so both {{math|?(x)}} and {{math|?<sup>-1</sup>(x)}} may be implemented as mapping between them.

First we define tools to handle trees.

<lang haskell>{-# language DeriveFunctor #-}
import Data.List (unfoldr)
import Data.Ratio

data Tree a = Node (Tree a) a (Tree a)
  deriving Functor

node (Node _ a _ ) = a

-- smart constructor
mkTree :: (a -> a -> a) -> a -> a -> Tree a
mkTree f l r = Node (mkTree f l m) m (mkTree f m r)
  where m = f l r

-- path to given element in the tree
track :: (Num a, Ord a) => Tree a -> a -> [Int]
track t x = unfoldr go t ++ [1]
  where
    go (Node l a r) = case compare x a of
        LT -> Just (0, l)
        EQ -> Nothing
        GT -> Just (1, r)

-- following the path in the tree
follow :: Tree a -> [Int] -> a
follow t lst =  node $ foldl go t $ init lst
  where
    go (Node l a r) x = [l, r] !! x

-- list of levels of the tree
levels :: Tree a -> [[a]]
levels (Node l a r) = [a] : zipWith (++) (levels l) (levels r)

--------------------------------------------------------------------------------
-- some type transformators and helper functions

toFraction :: RealFrac a => a -> (Int, a)
toFraction = properFraction

fromFraction :: RealFrac a => (Int, a) -> a
fromFraction (i, f) = fromIntegral i + f

toFloat :: Fractional a =>  (Int, Int) -> a
toFloat (a, b) = fromIntegral a / fromIntegral b

toRatio (a, b) = a % b
fromRatio r = (numerator r, denominator r)

oddFunc f 0 = 0
oddFunc f x = signum x * f (abs x)</lang>

Now it is possible to define two trees:

<lang haskell>mean (a,b) (c,d) = (a*d + c*b, 2*b*d)
mediant (a,b) (c,d) = (a + c, b + d)

farey :: Tree (Ratio Integer)  
farey = toRatio <$> mkTree mediant (0, 1) (1, 1)
   
minkowski :: Tree (Ratio Integer)  
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

<lang haskell>minkowskiQR :: Ratio Integer -> Ratio Integer
minkowskiQR = fromFraction . fmap transform . toFraction
  where
    transform = oddFunc (follow minkowski . track farey)

invMinkowskiQR :: Ratio Integer -> Ratio Integer
invMinkowskiQR = fromFraction . fmap transform . toFraction
  where
    transform = oddFunc (follow farey . track minkowski)</lang>

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

=== Using diadic numerals ===

Paths leading to numbers in Farey tree, give diadic representation of corresponding value of Minkowski ?-function and it's argument. So it is possible to use Farey tree to define Minkowski function for floating point numbers.

<lang haskell>fromDiadic :: (Int, [Int]) -> Double
fromDiadic = fromFraction . fmap (foldr go 0 . take 55)
  where
    go x r = (r + fromIntegral x)/2

toDiadic :: Double -> (Int, [Int])
toDiadic = fmap (unfoldr go) . toFraction
  where
    go x = case properFraction (x * 2) of
             (0, 0) -> Nothing
             (i, f) -> Just (i `mod` 2, f)

minkowskiQF :: Double -> Double
minkowskiQF = oddFunc $ fromDiadic . fmap transform . toFraction
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
