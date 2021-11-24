{-# language DeriveFunctor #-}
import Data.List

data MediantTree a = Node { left :: MediantTree a
                          , node :: a
                          , right :: MediantTree a }
  deriving Functor

mkTree (a,b) (c,d) =
  let m = (a+c, b+d)
  in Node (mkTree (a,b) m) m (mkTree m (c,d))

levels :: MediantTree a -> [[a]]
levels (Node l a r) =
  [a] : zipWith (++) (levels l) (levels r)

track n t x = take n $ go t
  where
    go (Node l a r) = if x < a then 0 : go l else 1 : go r

follow n t =
  node . foldl (\(Node l a r) x -> [l,r] !! x) t . take n

--------------------------------------------------------------------------------

sternBrocot :: MediantTree Double
sternBrocot =
  (\(a,b) -> fromIntegral a / fromIntegral b) <$> mkTree (0,1) (1,0)

minkowskiQF :: Double -> Double
minkowskiQF =
  fromDiadic . fmap (track 60 sternBrocot) . properFraction

invMinkowskiQF :: Double -> Double
invMinkowskiQF =
  fromFraction . fmap (follow 60 sternBrocot) . toDiadic
    

--------------------------------------------------------------------------------

fromFraction (i,f) = fromIntegral i + f

fromDiadic :: (Int, [Int]) -> Double
fromDiadic = fromFraction . fmap (foldr go 0)
  where
    go x r = (r + fromIntegral x)/2

toDiadic :: Double -> (Int, [Int])
toDiadic = fmap (unfoldr go) . properFraction
  where
    go x = case properFraction (x * 2) of
             (0, 0) -> Nothing
             (i, f) -> Just (i `mod` 2, f)
    

--------------------------------------------------------------------------------

farey :: MediantTree (Int, Int)
farey = mkTree (0,1) (1,1)

adjacent (a,b) = go $ (0,1) : (levels farey !! (b - 2)) ++ [(1,1)]
  where
    go (x:y:z:t) | y == (a,b) = (x,z)
                 | otherwise  = go (y:z:t)
