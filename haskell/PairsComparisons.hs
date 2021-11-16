{-# language DeriveFoldable #-}
import Control.Monad
import ListM (sortByM, insertByM)
import Data.Bool (bool)
import Data.Monoid
import Data.List
import Data.Foldable (toList)

--------------------------------------------------------------------------------
isortM, msortM, tsortM :: Monad m => (a -> a -> m Ordering) -> [a] -> m [a]

isortM cmp = foldM (flip (insertByM cmp)) []

msortM = sortByM

data BTree a = Empty | Node (BTree a) [a] (BTree a)
  deriving (Show, Foldable)

tsortM cmp = fmap toList . foldM add Empty
  where
    add Empty x = pure (Node Empty [x] Empty)
    add (Node l (a:as) g) x = do
      c <- cmp x a
      case c of
        LT -> Node <$> add l x <*> pure (a:as) <*> pure g
        EQ -> pure (Node l (x:a:as) g)
        GT -> Node l (a:as) <$> add g x
  
ask :: (Show a) => a -> a -> IO Ordering
ask a b = do
  putStr $ show a ++ " ≤ " ++ show b ++ " ? [y/n]  "
  ("y" ==) <$> getLine >>= return . bool GT LT

--------------------------------------------------------------------------------

median lst = sort lst !! (length lst `div` 2)
mean lst = sum (fromIntegral <$> lst) / genericLength lst

countComparisons cmp a b = (Sum 1, a `cmp` b)

fromList lst a b = compare (elemIndex a lst) (elemIndex b lst)

colors = ["Violet", "Red", "Green", "Indigo", "Blue", "Yellow", "Orange"]
sorted = ["Red", "Orange", "Yellow", "Green", "Blue", "Indigo", "Violet"]

hist lst = (\x -> (head x, length x)) <$> group (sort lst)

showHist (n, l) = putStrLn $ show n ++ "\t" ++ bar ++ " " ++ show perc ++ "%"
  where
    bar = replicate (max perc 1) '*'
    perc = (100 * l) `div` product [1..9]

test method = do
  mapM_ showHist $ hist res
  putStrLn $ "Median number of comparisons: " ++ show (median res)
  putStrLn $ "Mean number of comparisons: " ++ show (mean res)
  where
    res = getSum . fst . method (countComparisons compare) <$> permutations [1..9]

test2 method = hist $ concat res
  where
    res = method (\a b -> [compare a b]) <$> permutations [1..7]

test3 = getSum . fst . run <$> [tsortM, isortM, msortM]
  where
    run m = m (countComparisons (fromList sorted)) colors
   
main = test isortM

