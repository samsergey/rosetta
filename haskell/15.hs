{-# language DeriveFunctor #-}
{-# language TupleSections #-}

import Data.Maybe
import Data.Map (Map (..))
import qualified Data.Map as Map
import Control.Applicative
import Control.Monad
import Graph (Graph (..))
import Align (levenshteinDistance)

data Zip a = Zip [a] [a] deriving (Show, Functor, Eq, Ord) 

fromList :: [a] -> Zip a
fromList [] = Zip [] []
fromList (h:t) = Zip [h] t

toList :: Zip a -> [a]
toList (Zip l r) = reverse l ++ r

shiftL :: Zip a -> Zip a
shiftL (Zip (x:l) r) = Zip l (x:r)
shiftL z = z

shiftR :: Zip a -> Zip a
shiftR (Zip l (x:r)) = Zip (x:l) r
shiftR z = z

------------------------------------------------------------

type Board a = Zip (Zip a)

board :: [[a]] -> Board a
board lst = fromList $ fromList <$> lst

brd = board [[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,0]]

flatten = toList >=> toList

------------------------------------------------------------

data Move = R | L | U | D deriving (Show, Eq) 

move m b = fromMaybe b $ moveA b m

moveA :: Alternative f => Board a -> Move -> f (Board a)
moveA b m = case m of
  L -> case shiftL <$> b of
    Zip (Zip (x:xs) (y:ys) : l) r ->
      pure $ Zip (Zip (y:xs) (x:ys) : l) r
    _ -> empty
  R -> case b of
    Zip (Zip (x:xs) (y:ys) : l) r ->
      pure $ shiftR <$> Zip (Zip (y:xs) (x:ys) : l) r
    _ -> empty
  U -> case shiftL b of
    Zip (Zip (x:xs) a : l) (Zip (y:ys) b : r) ->
      pure $ Zip (Zip (y:xs) a : l) (Zip (x:ys) b : r)
    _ -> empty
  D -> case b of
    Zip (Zip (x:xs) a : l) (Zip (y:ys) b : r) ->
      pure $ shiftR $ Zip (Zip (y:xs) a : l) (Zip (x:ys) b : r)
    _ -> empty

gameGraph :: Ord a => Graph (Board a)
gameGraph = Graph $ \b -> Map.fromList ((,1) <$> ([R,L,U,D] >>= moveA b))                                        
distL2 b1 b2 = sum $ map (^2) $ zipWith (-) (flatten b1) (flatten b2)
distLv b1 b2 = levenshteinDistance (flatten b1) (flatten b2)
distL1 b1 b2 = sum $ map abs $ zipWith (-) (flatten b1) (flatten b2)
distLinf b1 b2 = maximum $ map abs $ zipWith (-) (flatten b1) (flatten b2)

start = board [[15, 14,  1,  6],[ 9, 11,  4, 12],[ 0, 10,  7,  3], [13,  8,  5,  2]]

prnt b = do
  mapM_ print $ toList $ toList <$> b
  putStr "\n"
