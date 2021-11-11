import System.IO (readFile)
import Data.List (intercalate, elemIndices)
import AStar (findPath, Graph(..))
import Data.Map (fromList)

distance :: String -> String -> Int
distance s1 s2 = length $ filter not $ zipWith (==) s1 s2

wordLadder :: [String] -> String -> String -> [String]
wordLadder dict start end = findPath g distance start end
  where
    short_dict = filter ((length start ==) . length) dict
    g = Graph $ \w -> fromList [ (x, 1)
                               | x <- short_dict
                               , distance w x == 1 ]

showChain [] = putStrLn "No chain"
showChain ch = putStrLn $ intercalate " -> " ch

main = do
  dict <- lines <$> readFile "unixdict.txt"
  showChain $ wordLadder dict "boy" "man"
  showChain $ wordLadder dict "girl" "lady"
  showChain $ wordLadder dict "john" "jane"
  showChain $ wordLadder dict "alien" "drool"
  showChain $ wordLadder dict "child" "adult"
