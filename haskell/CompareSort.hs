import Data.Array
import Data.Time.Clock
import Data.List
import Data.Maybe (fromMaybe)
import Control.Monad

--timed :: IO a -> IO (a, Int)
timed io = do
  t0 <- getCurrentTime
  x <- io
  t1 <- x `seq` getCurrentTime
  return (x, round $ 1000000 * diffUTCTime t1 t0)

ones = repeat 1
seqn = [1..]
rand = (`mod` 100) <$> iterate step 42
  where
    step x = (x * a + c) `mod` m
    (a, c, m) = (1103515245, 12345, 2^31-1)

test srt set = mapM (timed . run) [3..12]
  where
    run n = pure $ length $ srt (take (2^n) set) 

barChart ch lst = bar . scale <$> lst
  where
    scale (x,y) = (x, round $ logBase 2 $ fromIntegral y)
    bar (x,y) = show x ++ "\t" ++ replicate y ' ' ++ [ch]

overlap s1 s2 = take n $ zipWith f (pad s1) (pad s2)
  where
    f c ' ' = c
    f _ y   = y
    pad = (++ repeat ' ')
    n = length s1 `max` length s2

comparison set s1 s2 = do
  lst1 <- barChart '+' <$> test s1 set
  lst2 <- barChart 'x' <$> test s2 set
  mapM_ putStrLn $ zipWith overlap lst1 lst2

 
------------------------------------------------------------

-- Naive quick sort
qsort [] = []
qsort [x] = [x]
qsort (h:t) = qsort (filter (< h) t) ++ [h] ++ qsort (filter (>= h) t)

-- Bubble sort
bsort :: Ord a => [a] -> [a]
bsort s = case _bsort s of
               t | t == s    -> t
                 | otherwise -> bsort t
  where _bsort (x:x2:xs) | x > x2    = x2:_bsort (x:xs)
                         | otherwise = x :_bsort (x2:xs)
        _bsort s = s

-- Insertion sort
isort :: Ord a => [a] -> [a]
isort = foldr insert []

countingSort :: (Ix n) => n -> n -> [n] -> [n]
countingSort lo hi l = concatMap (uncurry $ flip replicate) count
  where count = assocs . accumArray (+) 0 (lo, hi) . map (\i -> (i, 1)) $ l

  
main = pure ()
