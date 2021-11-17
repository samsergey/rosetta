import Control.Monad
import ListM (sortByM, insertByM, partitionM, minimumByM)
import Data.Bool (bool)
import Data.Monoid
import Data.List

--------------------------------------------------------------------------------
isortM, msortM, tsortM :: Monad m => (a -> a -> m Ordering) -> [a] -> m [a]

-- merge sort
msortM = sortByM

-- insertion sort
isortM cmp = foldM (flip (insertByM cmp)) []

-- tree sort
tsortM cmp = go
  where
    go [] = pure []
    go (h:t) = do (l, g) <- partitionM (fmap (LT /=) . cmp h) t
                  go l <+> pure [h] <+> go g
    (<+>) = liftM2 (++)


-- interactive comparison
ask :: (Show a) => a -> a -> IO Ordering
ask a b = do
  putStr $ show a ++ " ≤ " ++ show b ++ " ? [y/n]  "
  bool GT LT . ("y" ==) <$> getLine

--------------------------------------------------------------------------------

countComparisons cmp a b = (Sum 1, a `cmp` b)

median lst = sort lst !! (length lst `div` 2)

mean lst = sum (fromIntegral <$> lst) / genericLength lst

hist lst = (\x -> (head x, length x)) <$> group (sort lst)

showHist (n, l) = putStrLn $ show n ++ "\t" ++ bar ++ " " ++ show perc ++ "%"
  where
    bar = replicate (max perc 1) '*'
    perc = (100 * l) `div` product [1..7]



fromList lst a b = compare (elemIndex a lst) (elemIndex b lst)
colors = ["Violet", "Red", "Green", "Indigo", "Blue", "Yellow", "Orange"]
sorted = ["Red", "Orange", "Yellow", "Green", "Blue", "Indigo", "Violet"]


test method = do
  mapM_ showHist $ hist res
  putStrLn $ "Median number of comparisons: " ++ show (median res)
  putStrLn $ "Mean number of comparisons: " ++ show (mean res)
  where
    res = getSum . fst . method (countComparisons compare) <$> permutations [1..7]


main = mapM_ test [tsortM, isortM, msortM]

--------------------------------------------------------------------------------

test2 method = hist $ concat res
  where
    res = method (\a b -> [compare a b]) <$> permutations [1..7]

test3 = getSum . fst . run <$> [tsortM, isortM, msortM]
  where
    run m = m (countComparisons (fromList sorted)) colors
   

