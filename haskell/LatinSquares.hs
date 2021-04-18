import Data.List (permutations, (\\), unfoldr)
import Control.Monad (foldM, forM_, msum)
import Data.Foldable (asum)
import Control.Applicative
import Control.Monad
 
latinSquares :: (Alternative f, Monad f) => Eq a => [a] -> f [[a]]
latinSquares [] = empty
latinSquares set = fmap reverse <$> squares
  where
    squares = foldM addRow firstRow perm
    perm = tail (groupedPermutations set)
    firstRow = pure <$> set
    addRow tbl rows = asum [ pure $ zipWith (:) row tbl
                           | row <- rows                      
                           , and $ different (tail row) (tail tbl) ]
    different = zipWith $ (not .) . elem
 
groupedPermutations :: Eq a => [a] -> [[[a]]]
groupedPermutations lst = map (\x -> (x :) <$> permutations (lst \\ [x])) lst
 
printTable :: Show a => [[a]] -> IO () 
printTable tbl = putStrLn $ unlines $ unwords . map show <$> tbl

-- deterministic latin square generator
-- uses the first row and the first column
latinSquare :: Eq a => [a] -> [a] -> [[a]]
latinSquare [] [] = []
latinSquare c r | head r /= head c = []
                | otherwise = reverse <$> foldl addRow firstRow perms
  where
    -- permutations grouped by the first element
    perms = tail $ (\x -> (x :) <$> permutations (r \\ [x])) <$> c
    firstRow = pure <$> r
    addRow tbl rows = head [ zipWith (:) row tbl
                           | row <- rows                      
                           , and $ different (tail row) (tail tbl) ]
    different = zipWith $ (not .) . elem

------------------------------------------------------------

{- Pure functional Haskell encourages programmer to separate
randomness from deterministic business logics. So first we determine
a function which returns a latin square build according to
given first row and first column.

<pre>λ> printTable $ latinSquare [1,2,3,4,5] [1,3,2,5,4]
1 2 3 4 5
3 4 1 5 2
2 5 4 3 1
5 3 2 1 4
4 1 5 2 3</pre>

Now whatever random generator is used, the construction of a random
latin square may be done by feeding two appropriate random permutations to
the deterministic algorythm.

For examples a naiive linear congruent method in a State monad is used.
-} 

data State s a = State { runState :: s -> (s, a) }

evalState st = snd . runState st

instance Functor (State s) where
  fmap f (State p) = State $ \s -> f <$> p s

instance Applicative (State s) where
  pure x  = State $ \s -> (s, x)
  (<*>) = ap
  
instance Monad (State s) where
  State p >>= f = State $ \s -> let ~(s', y) = p s
                                in runState (f y) s'
get :: State s s
get = State $ \s -> (s, s)

set :: s -> State s s
set s = State $ const (s, s)

modify :: (s -> s) -> State s s
modify f = get >>= set . f

type Random a = State Int a

random :: Integral a => a -> Random a
random k = rescale <$> modify iter 
  where
    iter x = (x * a + c) `mod` m
    (a, c, m) = (1103515245, 12345, 2^31-1)
    rescale x = fromIntegral x `mod` k

randomSample :: [a] -> Random a
randomSample lst = (lst !!) <$> random (length lst)

randomPermutation :: Eq a => [a] -> Random [a]
randomPermutation = go
  where
    go [] = pure []
    go lst = do
      x <- randomSample lst
      (x :) <$> go (lst \\ [x])

randomLatinSquare :: Eq a => [a] -> Random [[a]]
randomLatinSquare set = do
  r <- randomPermutation set
  c <- randomPermutation (tail r)
  return $ latinSquare r (head r:c)

main = printTable $ randomLatinSquare [0..6] `evalState` 42
