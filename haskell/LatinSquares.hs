import Data.List (permutations, (\\))
import Control.Monad (foldM, forM_, msum)
import Data.Foldable (asum)
import Control.Applicative
import Control.MonadRandom
 
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

latinSquare :: Eq a => [a] -> [[a]]
latinSquares [] = []
latinSquares set = [set]
