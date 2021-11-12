import Data.List (union)
import Text.Printf

-- search strategies
total [] = []
total (x:xs) = brauer (x:xs) `union` total xs

brauer [] = []
brauer (x:xs) = take 5 $ map (+ x) (x:xs)

truncatedBrauer n [] = []
truncatedBrauer n (x:xs) = take n $ map (+ x) (x:xs)

-- generation of chains with given strategy
chains _ 1 = [[1]]
chains sums n = go [[1]]
  where
    go r = let r' = r >>= step
               complete = filter ((== n) . head) r'
           in if null complete then go r' else complete

    step ch = (: ch) <$> filter (\s -> s > head ch && s <= n) (sums ch)

-- the predicate for Brauer chains
isBrauer [_] = True
isBrauer [_,_] = True
isBrauer (x:y:xs) = (x - y) `elem` (y:xs) && isBrauer (y:xs)


task :: Int -> IO()
task n =
  let ch = chains total n
      br = filter isBrauer ch
      nbr = filter (not . isBrauer) ch
  in do
    printf "L(%d) = %d\n" n (length (head ch) - 1)
    printf "Brauer chains(%i)\t: count = %i\tEx: %s\n" n (length br) (show $ reverse $ head br)
    if not $ null nbr
      then
      printf "non-Brauer chains(%i)\t: count = %i\tEx: %s\n\n" n (length ch - length br) (show $ reverse $ head nbr)
      else
      putStrLn "No non Brauer chains\n"

extraTask :: Int -> IO()
extraTask n =
  let ch = chains brauer n
  in do
    printf "L(%d) = %d\n" n (length (head ch) - 1)
    printf "Brauer chains(%i)\t: count = %i\tEx: %s\n" n (length ch) (show $ reverse $ head ch)
    putStrLn "Non-Brauer analysis suppressed\n"


main = print $ head $ chains (truncatedBrauer 3) 12509
  
