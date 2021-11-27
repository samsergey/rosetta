{-# LANGUAGE BangPatterns #-}
import Data.Tree
import Data.Ratio

intervalTree :: (a -> a -> a) -> (a, a) -> Tree a
intervalTree node = unfoldTree $
  \(!a, !b) -> let m = node a b
               in (m, [(a,m), (m,b)])

sternBrocot :: Tree Rational
sternBrocot = mirror $ toRatio <$> intervalTree mediant ((0,1), (1,0))

mediant (!p, !q) (!r, !s) = (p + r, q + s)
toRatio (!p, !q) = p % q
toFloat (!p, !q) = p / q

minkowski :: Tree Rational
minkowski = mirror $ toRatio <$> intervalTree mean ((0,1), (1,0))

mean (!p, !q) (1, 0) = (p+1, q)
mean (!p, !q) (!r, !s) = (p*s + q*r, 2*q*s)

Node a [l1, r1] ==> Node b [l2, r2] =
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
minkowskiF = mirror $ fromRational . toRatio <$> intervalTree mean ((0,1), (1,0))

minkowskiQF, invMinkowskiQF :: Double -> Double
minkowskiQF = sternBrocotF ==> minkowskiF
invMinkowskiQF = minkowskiF ==> sternBrocotF

mirror :: Num a => Tree a -> Tree a
mirror t = Node 0 [reflect (negate <$> t), t]
  where
    reflect (Node a [l,r]) = Node a [reflect r, reflect l]

stopOn stop (Node a [l,r])
  | stop a (rootLabel l) (rootLabel r) = Node a []
  | otherwise = Node a $ stopOn stop <$> [l,r]

small _ a b = abs (a - b)*10^13 < abs (a + b)

track (Node a [l, r]) x =
  case x `compare` a of
    LT -> 0 : track l x
    EQ -> [1]
    GT -> 1 : track r x

follow t = fmap rootLabel . scanr (\x t -> subForest t !! x) t . reverse

fromDiadic s = foldr go 0 s
  where
    go x r = (fromIntegral x + r)/2
