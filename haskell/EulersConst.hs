import Data.List
import Data.Monoid
import Data.Semigroup

limit :: Double -> [Double] -> Maybe (Int, Double)
limit eps lst = case span large pairs  of
                (_,[]) -> Nothing
                (s, _) -> Just (length s, snd (last s)) 
  where
    large (a,b) = abs (a-b) > abs (a+b) || abs (a - b) > eps
    dist (x1,x2) = abs (x1 - x2)
    pairs = take 10000 $ zip lst (tail lst)

sums = scanl (+) 0
products = scanl (*) 1

infixl 3 .+., .-.
(.-.) = zipWith (-)
(.+.) = zipWith (+)
infixl 4 .*., *., ./.
(.*.) = zipWith (*)
(./.) = zipWith (/)
a *. lst = map (* a) lst

nats = [1..]

harmonics = recip <$> nats

harmonicNumbers = sums harmonics

directEstimation = harmonicNumbers .-. log <$> nats

sweeneyEstimation' = go 2 n (0, n)
  where
    n = 21
    go k r (a, b) = (b - a - log n) : go (k+1) (r*n/k) (b, a + r/k) 
   

clipBy n = unfoldr (Just . splitAt n)
