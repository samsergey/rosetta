{-# LANGUAGE FlexibleContexts #-}
import Data.List (union, tails, minimumBy, init)
import Data.Ord
 
-- search strategies
total [] = []
total (x:xs) = brauer (x:xs) `union` total xs
 
brauer [] = []
brauer (x:xs) = map (+ x) (x:xs)
 
-- generation of chains with given strategy
minimalChains _ 1 = [[1]]
minimalChains sums n = go [[1]]
  where
    go ch = let next = ch >>= step
                complete = filter ((== n) . head) next
            in if null complete then go next else complete
 
    step ch = (: ch) <$> filter (\s -> s > head ch && s <= n) (sums ch)
 
-- the predicate for Brauer chains
isBrauer [_] = True
isBrauer [_,_] = True
isBrauer (x:y:xs) = (x - y) `elem` (y:xs) && isBrauer (y:xs)

isChain [] = False
isChain [_] = True
isChain [x,y] = x == 2*y
isChain (x:xs) = any (\y -> (x - y) `elem` xs) xs && isChain xs

chain 1 = [1]
chain n | n < 100 = head $ minimalChains brauer n
        | otherwise = let a = round (sqrt (fromIntegral n))
                          (q, r) = n `divMod` a 
                      in chain r `addCh` (chain a `mulCh` chain q)

mulCh c1 c2 = map (head c2 *) c1 ++ tail c2
addCh c1 c2 = map (head c2 +) c1 ++ c2

binaryChain 1 = [1]
binaryChain n | even n = n : binaryChain (n `div` 2)
              | odd n = n : binaryChain (n - 1)

minChain :: (Int -> [Int]) -> Int -> [Int]
minChain strategy = go
  where
    go :: Int -> [Int]
    go 3 = [3,2,1]
    go n = case log2 n of
      Just a -> a
      Nothing -> minimumBy (comparing length) $ map (\k -> chain [k, n]) $ strategy n

    chain :: [Int] -> [Int]
    chain ch
      | n2 <= 1 = go n1
      | otherwise = case n1 `divMod` n2 of
                      (q, 0) -> go q `mulCh` chain (init ch) `mulCh` go q
                      (q, r) -> [r] `addCh` (go q `mulCh` chain (r : init ch))
      where (n1:n2:ns) = reverse ch


binary n = [n `div` 2]

log2 = go
  where
    go 1 = Just [1]
    go n | even n = (n :) <$> go (n `div` 2)
         | odd n = Nothing

primes = 2 : sieve [3,5..]
  where sieve (x:xs) = x : sieve (filter (\y -> y `mod` x /= 0) xs)

