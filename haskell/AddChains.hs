import Data.List
import Data.Monoid

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
 
binaryChain 1 = [1]
binaryChain n | even n = n : binaryChain (n `div` 2)
              | odd n = n : binaryChain (n - 1)
 
dichotomicChain n
  | n == 3  = [3, 2, 1]
  | n == 2 ^ log2 n = takeWhile (> 0) $ iterate (`div` 2) n
  | otherwise = let k = n `div` (2 ^ ((log2 n + 1) `div` 2))
                in chain n k
  where
    chain n1 n2 
      | n2 <= 1 = dichotomicChain n1
      | otherwise = case n1 `divMod` n2 of
          (q, 0) -> dichotomicChain q `mul` dichotomicChain n2
          (q, r) -> [r] `add` (dichotomicChain q `mul` chain n2 r)

    c1 `mul` c2 = map (head c2 *) c1 ++ tail c2
    c1 `add` c2 = map (head c2 +) c1 ++ c2
 
    log2 = floor . logBase 2 . fromIntegral

0 `times` _ = mempty
n `times` x = res
  where
    (res:_, _, _) = foldl f ([x], 1, 0) $ tail ch
    ch = reverse $ binaryChain n -- dichotomicChain n
    f (p:ps, c1, i) c2 = let Just j = elemIndex (c2-c1) ch
                         in ((p <> ((p:ps) !! (i-j))):p:ps, c2, i+1)

data M a = M [[a]] | I deriving Show

instance Num a => Semigroup (M a) where
  I <> m = m
  m <> I = m
  M m1 <> M m2 = M $ map (\r -> map (\c -> r `dot` c) (transpose m2)) m1
    where dot a b = sum $ zipWith (*) a b

instance Num a => Monoid (M a) where
  mempty = I



a = let s = sqrt (1/2)
    in M [[s,0,s,0,0,0]
         ,[0,s,0,s,0,0]
         ,[0,s,0,-s,0,0]
         ,[-s,0,s,0,0,0]
         ,[0,0,0,0,0,1]
         ,[0,0,0,0,1,0]]

main = print "Ok"
