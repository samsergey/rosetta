<<<<<<< HEAD
import Data.Array
import Data.Time.Clock
import Data.List
import Data.Maybe (fromMaybe)
import Control.Monad

--timed :: IO a -> IO (a, Int)
=======
import Data.Time.Clock
import Data.List

type Time = Integer
type Sorter a = [a] -> [a]

timed :: IO a -> IO (a, Time)
>>>>>>> 231348938c4036a6d68d0d625c9b3c476363cc15
timed io = do
  t0 <- getCurrentTime
  x <- io
  t1 <- x `seq` getCurrentTime
<<<<<<< HEAD
  return (x, round $ 1000000 * diffUTCTime t1 t0)

=======
  return (x, ceiling $ 1000000 * diffUTCTime t1 t0)
 
>>>>>>> 231348938c4036a6d68d0d625c9b3c476363cc15
ones = repeat 1
seqn = [1..]
rand = (`mod` 100) <$> iterate step 42
  where
    step x = (x * a + c) `mod` m
    (a, c, m) = (1103515245, 12345, 2^31-1)

<<<<<<< HEAD
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

=======
test :: [a] -> Sorter a -> IO [(Int, Time)]
test set srt = mapM (timed . run) ns
  where
    ns = take 15 $ iterate (\x -> (x * 5) `div` 3) 10
    run n = pure $ length $ srt (take n set) 

------------------------------------------------------------
barChart :: Char -> [(Int, Time)] -> [String]
barChart ch lst = bar . scale <$> lst
  where
    scale (x,y) = (x, round $ (3*) $ log $ fromIntegral y)
    bar (x,y) = show x ++ "\t" ++ replicate y ' ' ++ [ch]

over :: String -> String -> String
over s1 s2 = take n $ zipWith f (pad s1) (pad s2)
  where
    f ' ' c = c
    f c ' ' = c
    f y _   = y
    pad = (++ repeat ' ')
    n = length s1 `max` length s2

comparison :: Ord a => [Sorter a] -> [Char] -> [a] -> IO ()
comparison sortings chars set = do
  results <- mapM (test set) sortings
  let charts = zipWith barChart chars results
  putStrLn $ replicate 50 '-'
  mapM_ putStrLn $ foldl1 (zipWith over) charts
  putStrLn $ replicate 50 '-'
  let times = map (fromInteger . snd) <$> results
  let ratios = mean . zipWith (flip (/)) (head times) <$> times
  putStrLn "Comparing average time ratios:"
  mapM_ putStrLn $ zipWith (\r s -> [s] ++ ": " ++ show r) ratios chars
  where
    mean lst = sum lst / genericLength lst
    
 
------------------------------------------------------------
 
-- Naive quick sort
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (h:t) = qsort (filter (< h) t) ++ [h] ++ qsort (filter (>= h) t)
 
>>>>>>> 231348938c4036a6d68d0d625c9b3c476363cc15
-- Bubble sort
bsort :: Ord a => [a] -> [a]
bsort s = case _bsort s of
               t | t == s    -> t
                 | otherwise -> bsort t
  where _bsort (x:x2:xs) | x > x2    = x2:_bsort (x:xs)
                         | otherwise = x :_bsort (x2:xs)
        _bsort s = s
<<<<<<< HEAD

-- Insertion sort
isort :: Ord a => [a] -> [a]
isort = foldr insert []

countingSort :: (Ix n) => n -> n -> [n] -> [n]
countingSort lo hi l = concatMap (uncurry $ flip replicate) count
  where count = assocs . accumArray (+) 0 (lo, hi) . map (\i -> (i, 1)) $ l

  
main = pure ()
=======
 
-- Insertion sort
isort :: Ord a => [a] -> [a]
isort = foldr insert []
 

main = do
  putStrLn "comparing on list of ones"
  run ones
  putStrLn "\ncomparing on presorted list"
  run seqn
  putStrLn "\ncomparing on suffled list"
  run rand
  where
    run = comparison [sort, isort, qsort, bsort] "siqb"
>>>>>>> 231348938c4036a6d68d0d625c9b3c476363cc15
