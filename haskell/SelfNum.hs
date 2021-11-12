import Data.Semigroup
import Control.Arrow
import Control.Monad

gcdarr :: [Int] -> Int
gcdarr arr = gcd a b
  where (Min a, Max b) = foldMap (\x -> (Min x, Max x)) arr

gcdarr' :: [Int] -> Int
gcdarr' = foldMap (Min &&& Max) >>> (getMin *** getMax) >>> uncurry gcd

gcdarr'' :: [Int] -> Int
gcdarr'' = liftM2 gcd minimum maximum

