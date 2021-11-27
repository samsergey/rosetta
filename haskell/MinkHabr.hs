import Data.Tree
import Data.Ratio

intervalTree :: (a -> a -> a) -> (a, a) -> Tree a
intervalTree node = unfoldTree $
  \(a, b) -> let m = node a b in (m, [(a,m), (m,b)])

Node a _ ==> Node b [] = const b
Node a [] ==> Node b _ = const b
Node a [l1, r1] ==> Node b [l2, r2] =
  \x -> case x `compare` a of
          LT -> (l1 ==> l2) x
          EQ -> b
          GT -> (r1 ==> r2) x

mirror :: Num a => Tree a -> Tree a
mirror t = Node 0 [reflect (negate <$> t), t]
  where
    reflect (Node a [l,r]) = Node a [reflect r, reflect l]

------------------------------------------------------------

sternBrocot :: Tree Rational
sternBrocot = toRatio <$> intervalTree mediant ((0,1), (1,0))
  where
    mediant (p, q) (r, s) = (p + r, q + s)

toRatio (p, q) = p % q

minkowski :: Tree Rational
minkowski = toRatio <$> intervalTree mean ((0,1), (1,0))

mean (p, q) (1, 0) = (p+1, q)
mean (p, q) (r, s) = (p*s + q*r, 2*q*s)


questionMark, invQuestionMark :: Rational -> Rational
questionMark    = mirror sternBrocot ==> mirror minkowski
invQuestionMark = mirror minkowski ==> mirror sternBrocot

------------------------------------------------------------

sternBrocotF :: Tree Double
sternBrocotF = mirror $ fromRational <$> sternBrocot

minkowskiF :: Tree Double
minkowskiF = mirror $ intervalTree mean (0, 1/0)
  where
    mean a b | isInfinite b = a + 1
             | otherwise = (a + b) / 2

questionMarkF, invQuestionMarkF :: Double -> Double
questionMarkF = sternBrocotF ==> minkowskiF
invQuestionMarkF = minkowskiF ==> sternBrocotF



terminateOn eps = go
  where
    go (Node a [l,r])
      | small (rootLabel l) (rootLabel r) = Node a []
      | otherwise = Node a [go l, go r]
    small a b = abs (a - b) < abs (a + b) * eps



------------------------------------------------------------
{-
questionMark . invQuestionMark = id
(sternBrocot ==> minkowski) . (minkowski ==> sternBrocot) $ x

Node a [l1, r1] ==> Node b [l2, r2] =
  \x -> case x `compare` a of
          LT -> (l1 ==> l2) x
          EQ -> b
          GT -> (r1 ==> r2) x

(Node a [l1, r1] ==> Node b [l2, r2]) . (Node b [l2, r2] ==> Node a [l1, r1]) $ x
LT (Node a [l1, r1] ==> Node b [l2, r2]) . (l2 ==> l1) $ x
GT (Node a [l1, r1] ==> Node b [l2, r2]) . (r2 ==> r1) $ x
EQ (Node a [l1, r1] ==> Node b [l2, r2]) . (Node b [l2, r2] ==> Node a [l1, r1]) $ b
EQ (Node a [l1, r1] ==> Node b [l2, r2]) b
EQ b
-}
------------------------------------------------------------

track :: Ord a => Tree a -> a -> [Int]
track (Node a [l, r]) x =
  case x `compare` a of
    LT -> 0 : track l x
    EQ -> [1]
    GT -> 1 : track r x

follow t = rootLabel . foldl (\t x -> subForest t !! x) t

fromDiadic :: [Int] -> Double
fromDiadic s = foldr (\x r -> fromIntegral x + r/2) 0 s



-- https://cp4space.hatsya.com/2012/09/04/closed-form-bijections/
