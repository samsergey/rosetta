import Control.Monad
import ListM
import Data.Bool (bool)
import Data.Monoid
import Data.List

--------------------------------------------------------------------------------

qsortM :: (Eq a, Monad m) => (a -> a -> m Bool) -> [a] -> m [a]
qsortM cmp = go
  where
    go [] = pure []
    go (h:t) = do let (hs, rest) = partition (== h) (h:t)
                  (g, l) <- partitionM (cmp h) rest
                  go l <+> pure hs <+> go g
    (<+>) = liftM2 (++)
    
--------------------------------------------------------------------------------

isortM :: (Eq a, Monad m) => (a -> a -> m Bool) -> [a] -> m [a]
isortM cmp = foldM insert []
  where
    insert [] x = pure [x]
    insert (h:t) x
      | x == h    = pure (x:h:t)
      | otherwise = cmp x h >>= bool ((h :) <$> insert t x) (pure (x:h:t))

--------------------------------------------------------------------------------

msortM :: (Eq a, Monad m) => (a -> a -> m Bool) -> [a] -> m [a]
msortM cmp = sortByM ord
  where
    ord a b | a == b = pure EQ
            | otherwise = bool GT LT <$> cmp a b

--------------------------------------------------------------------------------
    
ask :: (Show a) => a -> a -> IO Bool
ask a b = do putStr $ "Is pair " ++ show (a,b) ++ " ordered? [y/n] "
             ("y" == ) <$> getLine

median lst = sort lst !! (length lst `div` 2)
mean lst = sum (fromIntegral <$> lst) / genericLength lst

countComparisons cmp a b = (Sum 1, a `cmp` b)

colors = ["Violet", "Red", "Green", "Indigo", "Blue", "Yellow", "Orange"]

hist lst = (\x -> (head x, length x)) <$> group (sort lst)

showHist (n, l) = putStrLn $ show n ++ "\t" ++ bar ++ " " ++ show l
  where
    bar = replicate (max (l `div` 20) 1) '*'

test method = do
  mapM_ showHist $ hist res
  putStrLn $ "Median number of comparisons: " ++ show (median res)
  where
    res = getSum . fst . method (countComparisons (<=)) <$> permutations [1..7]
