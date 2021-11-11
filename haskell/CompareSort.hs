import Data.Time.Clock
import Data.List

type Time = Integer
type Sorter a = [a] -> [a]

timed :: IO a -> IO (a, Time)
timed io = do
  t0 <- getCurrentTime
  x <- io
  t1 <- x `seq` getCurrentTime
  return (x, ceiling $ 1000000 * diffUTCTime t1 t0)
 
ones = repeat 1
seqn = [1..]
rand = (`mod` 100) <$> iterate step 42
  where
    step x = (x * a + c) `mod` m
    (a, c, m) = (1103515245, 12345, 2^31-1)

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
 

main = do
  putStrLn "comparing on list of ones"
  run ones
  putStrLn "\ncomparing on presorted list"
  run seqn
  putStrLn "\ncomparing on suffled list"
  run rand
  where
    run = comparison [sort, isort, qsort, bsort] "siqb"
