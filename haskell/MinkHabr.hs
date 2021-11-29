{-# LANGUAGE DerivingVia #-}

import Data.Tree
import Data.Ratio
import Data.List

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


truncateOn :: (Ord a, Num a) => a -> Tree a -> Tree a
truncateOn eps (Node a [l,r])
  | small (rootLabel l, rootLabel r) = Node a []
  | otherwise = Node a $ truncateOn eps <$> [l,r]
  where
    small (a, b) = abs (a - b) < eps ||
                   abs (a - b) < abs (a + b) * eps

------------------------------------------------------------

newtype N = N Integer
  deriving (Eq, Num, Show) via Integer

nats :: Tree N
nats = unfoldTree (\b -> (N b, [2*b, 2*b+1])) 1

instance Ord N where
  compare (N x) (N n)
    | x == n    = EQ
    | x < m     = LT
    | otherwise = GT
    where
      m = 2^(level x - level n - 1) * (2*n + 1)
      level = floor . logBase 2 . fromIntegral

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

follow :: Tree a -> [Int] -> a
follow t = rootLabel . foldl (\t x -> subForest t !! x) t

toDiadic = tail . track minkowskiF
fromDiadic = follow minkowskiF . (1:)

listToInt lst = zip ((1+) <$> lst) (cycle [0,1]) >>= uncurry replicate 

fromBinary lst = foldl (\r x -> r*2 + x) 0 lst

toBinary = reverse . unfoldr go
  where go 0 = Nothing
        go n = let  (q,r) = n `divMod` 2 in Just (r,q)

toInt = fromBinary . track sternBrocot

nats' :: Tree Integer
nats' = Node 0 [(+1).(*2) <$> nats', (*2).(+1) <$> nats']

trackNat :: Integer -> [Int]
trackNat = go nats
  where
    go (Node _ [l, r]) n
      | n == 0 = []
      | odd n = 0 : go l (n `div` 2)
      | otherwise = 1 : go r (n `div` 2 - 1)


showTree 0 _ = mempty
showTree n (Node a f) = "Tree["++show a++",{"++ intercalate "," (showTree (n-1) <$> f) ++"}]"

graphTree 0 _ = []
graphTree n (Node a f) = (((show a <> " -> ") <>) . show.rootLabel <$> f) <> foldMap (graphTree (n-1)) f

toGraph n t = "Graph[{" ++ intercalate "," (graphTree n t) ++ "}]"
