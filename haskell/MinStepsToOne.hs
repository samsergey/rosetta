{-# LANGUAGE DeriveFunctor #-}
import Data.List
import Data.Ord
import Data.Function (on)

------------------------------------------------------------
-- memoization utilities

data Memo a = Node a (Memo a) (Memo a)
  deriving Functor

memo :: Integral a => Memo p -> a -> p
memo (Node a l r) n
  | n == 0 = a
  | odd n = memo l (n `div` 2)
  | otherwise = memo r (n `div` 2 - 1)

nats :: Integral a => Memo a
nats = Node 0 ((+1).(*2) <$> nats) ((*2).(+1) <$> nats)

memoize :: Integral a => (a -> b) -> (a -> b)
memoize f = memo (f <$> nats)

------------------------------------------------------------

data Step = Div Int | Sub Int
  deriving Show

run :: Int -> Step -> [(Step, Int)]
run n s = case s of
  Sub i | n > i -> [(s, n - i)]
  Div d | n `mod` d == 0 -> [(s, n `div` d)]
  _ -> []

minSteps :: [Step] -> Int -> (Int, [Step])
minSteps steps = go
  where
    go = memoize goM
    
    goM 1 = (0, [])
    goM n = minimumBy (comparing fst) $ do
      (s, k) <- steps >>= run n
      let (m, ss) = go k
      return (m+1, s:ss)

------------------------------------------------------------

showSteps :: Int -> [Step] -> String
showSteps = foldl go . show
  where
    go r (Div d) = r ++ "/" ++ show d
    go r (Sub s) = "(" ++ r ++ "-" ++ show s ++ ")"


task1 steps =  mapM_ (put . go) [1..10]
  where
    go n = showSteps n <$> minSteps steps n
    put (n,s) = putStrLn $ show n ++ ":\t" ++ s

task2 steps range = mapM_ put longest
  where
    put (n,(l,s)) = putStrLn $ show l ++ ": " ++
                    showSteps n s
    longest = 
      head $ groupBy ((==) `on` (fst.snd)) $
      sortOn (negate . fst . snd) $
      zip [1..] (minSteps steps <$> range)

