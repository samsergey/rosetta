{-# language FlexibleContexts #-}
import Data.List           (unfoldr, genericLength, elemIndices)
import System.Random       (StdGen, mkStdGen, randoms)
import Control.Monad.State (state, runState)
import Text.Printf         (printf)
import Data.Set            ((\\))
import qualified Data.Set  as S

clusters :: [[Bool]] -> [S.Set Int]
clusters m = unfoldr findCuster $ matrixToCells m
  where
    matrixToCells = S.fromList . elemIndices True . mconcat
    
    findCuster s = do
      (p, ps) <- S.minView s
      pure (runState (expand p) ps)
      
    expand p = do
      ns <- state $ extract (neigbours p)
      xs <- mapM expand $ S.elems ns
      return $ S.insert p $ mconcat xs

    extract s1 s2 = (s2 `S.intersection` s1, s2 \\ s1)
    neigbours c = S.fromList [c-1, c+1, c-n, c+n]
    n = length m

randomMatrices :: Int -> StdGen -> [[[Bool]]]
randomMatrices n = clipBy n . clipBy n . randoms
  where
    clipBy n = unfoldr (Just . splitAt n)

tests :: Int -> StdGen -> [Int]
tests n = map (length . clusters) . randomMatrices n

task :: Int -> StdGen -> Double
task n = mean . take 10 . map density . tests n 
  where
    density c = fromIntegral c / fromIntegral n**2
    mean lst = sum lst / genericLength lst
    
main = pure (mkStdGen 137) >>= mapM_ (printf "%f\n") . res
  where
    res = mapM task [4,16,64,128,512]
