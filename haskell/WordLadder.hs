import System.IO (readFile)
import Control.Monad
import Control.Applicative
import Data.List (intercalate, nub)
import Data.Maybe
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

    shrinkFrom = scanM (findM . oneStepAway)

    oneStepAway x = (1 ==) . distance x

    scanM f x = fmap snd . foldM g (x,[x])
      where g (b, r) a = (\x -> (x, x:r)) <$> f b a

    findM p = msum . map (\x -> if p x then pure x else mzero)

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
--  showChain $ wordLadder "alien" "drool" dict
  showChain $ wordLadder "child" "adult" dict
  showChain $ wordLadder "lead" "gold" dict
  showChain $ wordLadder "white" "black" dict
  showChain $ wordLadder "bubble" "tickle" dict

------------------------------------------------------------

test s e = do
  dict <- lines <$> readFile "unixdict.txt"
  return $  wordLadders s e dict

test2 s e = do
  dict <- lines <$> readFile "unixdict.txt"
  return $  wordLadders2 s e dict

wordLadders2 :: String -> String -> [String] -> [[String]]
wordLadders2 start end dict
  | length start /= length end = []
  | otherwise = pure wordSpace >>= expand start end >>= shrink end
  where
 
    wordSpace = S.fromList $ filter ((length start ==) . length) dict

    expand s e d = tail . map S.elems <$> go [S.singleton s] [S.singleton e] d
      where
        go (hs:ts) (he:te) d
          | S.null d || S.null fs || S.null fe = []
          | not $ S.null f1 = [reverse (f1:te) ++ hs:ts]
          | not $ S.null f2 = [reverse (he:te) ++ f2:ts]
          | not $ S.null f3 = [reverse (he:te) ++ f3:hs:ts]
          | otherwise = go (fs:hs:ts) (fe:he:te) (d S.\\ hs S.\\ he)
          where
            fs = front hs
            fe = front he
            f1 = fs `S.intersection` he
            f2 = fe `S.intersection` hs
            f3 = fs `S.intersection` fe
            front = S.foldr (\w -> S.union (S.filter (oneStepAway w) d)) mempty

    shrink = scanM (findM . oneStepAway)

    oneStepAway x = (1 ==) . distance x

    scanM f x = fmap snd . foldM g (x,[x])
      where g (b, r) a = (\x -> (x, x:r)) <$> f b a

    findM p = msum . map (\x -> if p x then pure x else mzero)
