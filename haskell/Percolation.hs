{-# language FlexibleContexts #-}
import Data.List
import System.Random
import Control.Monad.State
import Text.Printf
import qualified Data.Set as S

clipBy :: Int -> [a] -> [[a]]
clipBy n = unfoldr (Just . splitAt n)

matrixToCells m = S.fromList $
  [ (i,j) | (i,r) <- zip [0..] m
          , (j,x) <- zip [0..] r, x]

neigbours (i,j) = (`S.member` S.fromList [(i-1,j), (i,j-1), (i+1,j), (i,j+1)])

clusters = unfoldr findCuster . matrixToCells
  where
    findCuster s = do
      (p, ps) <- S.minView s
      pure (runState (go p) ps)
      
    go p = do
      (ns, ps') <- gets $ S.partition (neigbours p)
      put ps'
      xs <- mapM go (S.elems ns)
      return $ S.insert p (mconcat xs)

randomMatrices :: Int -> StdGen -> [[[Bool]]]
randomMatrices n = clipBy n . clipBy n . randoms

tests :: Int -> StdGen -> [Int]
tests n g = length . clusters <$> randomMatrices n g

experiment :: Int -> StdGen -> Double
experiment n g = (mean $ take 10 $ tests n g) / fromIntegral n**2
  where
    mean lst = fromIntegral (sum lst) / fromIntegral (length lst)
    
main = newStdGen >>= mapM_ (printf "%f\n") . res
  where
    res = mapM experiment [10,100,200]
