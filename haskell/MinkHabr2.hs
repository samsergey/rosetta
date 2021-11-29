{-# LANGUAGE BangPatterns, DeriveFunctor #-}
import Data.Ratio

data Tree a = Node (Tree a) !a (Tree a)
  deriving (Functor)

rootLabel (Node _ a _) = a

intervalTree :: (a -> a -> a) -> a -> a -> Tree a
intervalTree node !a !b =
  let m = node a b
  in Node (intervalTree node a m) m (intervalTree node m b)

sternBrocot :: Tree Rational
sternBrocot = mirror $ toRatio <$> intervalTree mediant (0,1) (1,0)

mediant (!p, !q) (!r, !s) = (p + r, q + s)

toRatio (!p, !q) = p % q
toFloat (!p, !q) = p / q

minkowski :: Tree Rational
minkowski = mirror $ toRatio <$> intervalTree mean (0,1) (1,0)

mean (!p, !q) (1, 0) = (p+1, q)
mean (!p, !q) (!r, !s) = (p*s + q*r, 2*q*s)

Node l1 a r1 ==> Node l2 b r2 =
  \x -> case x `compare` a of
          LT -> (l1 ==> l2) x
          EQ -> b
          GT -> (r1 ==> r2) x

minkowskiQR, invMinkowskiQR :: Rational -> Rational

minkowskiQR    = sternBrocot ==> minkowski
invMinkowskiQR = minkowski ==> sternBrocot

sternBrocotF :: Tree Double
sternBrocotF = fromRational <$> mirror sternBrocot

minkowskiF :: Tree Double
minkowskiF = mirror $ intervalTree mean 0 (1/0)
  where
    mean a b | isInfinite b = a
             | otherwise = (a + b) / 2

minkowskiQF, invMinkowskiQF :: Double -> Double
minkowskiQF = sternBrocotF ==> minkowskiF
invMinkowskiQF = minkowskiF ==> sternBrocotF

mirror :: Num a => Tree a -> Tree a
mirror t = Node (reflect $ negate <$> t) 0 t
  where
    reflect (Node l a r) = Node (reflect r) a (reflect l)

stopOn stop (Node a [l,r])
  | stop a (rootLabel l) (rootLabel r) = Node a []
  | otherwise = Node a $ stopOn stop <$> [l,r]

small _ a b = abs (a - b)*10^13 < abs (a + b)


track (Node l !a r) x =
  case x `compare` a of
    LT -> 0 : track l x
    EQ -> []
    GT -> 1 : track r x

follow t = rootLabel . foldl (\(Node l _ r) x -> [l,r] !! x) t

fromDiadic s = foldr go 0 s
  where
    go x r = (fromIntegral x + r)/2
