import System.IO (readFile)
import Control.Monad (foldM)
import Data.List (intercalate)
--import AStar (findPath, Graph(..))
--import qualified Data.Map as M
import qualified Data.Set as S

distance :: String -> String -> Int
distance s1 s2 = length $ filter not $ zipWith (==) s1 s2

-- wordLadder' :: [String] -> String -> String -> [String]
-- wordLadder' dict start end = findPath g distance start end
--   where
--     short_dict = filter ((length start ==) . length) dict
--     g = Graph $ \w -> M.fromList [ (x, 1)
--                                  | x <- short_dict
--                                  , distance w x == 1 ]

wordLadders :: String -> String -> [String] -> [[String]]
wordLadders start end dict
  | length start /= length end = []
  | otherwise = pure wordSpace >>= expandFrom start >>= shrinkFrom end
  where
 
    wordSpace = S.fromList $ filter ((length start ==) . length) dict

    expandFrom s = go [[s]]
      where
        go (h:t) d
          | S.null d || S.null f = []
          | end `S.member` f = [h:t]
          | otherwise = go (S.elems f:h:t) (d S.\\ f)
          where
            f = foldr (\w -> S.union (S.filter (oneStepAway w) d)) mempty h

    shrinkFrom = scanM (filter . oneStepAway)

    oneStepAway x = (1 ==) . distance x
    
    scanM f x = fmap snd . foldM g (x,[x])
      where g (b, r) a = (\x -> (x, x:r)) <$> f b a

wordLadder :: String -> String -> [String] -> [String]
wordLadder s e d = case wordLadders s e d of
                     [] -> []
                     h:_ -> h


showChain [] = putStrLn "No chain"
showChain ch = putStrLn $ intercalate " -> " ch

main = do
  dict <- lines <$> readFile "unixdict.txt"
  showChain $ wordLadder "boy" "man" dict
  showChain $ wordLadder "girl" "lady" dict
  showChain $ wordLadder "john" "jane" dict
  showChain $ wordLadder "alien" "drool" dict
  showChain $ wordLadder "child" "adult" dict
