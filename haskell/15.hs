{-# language DeriveFunctor #-}
{-# language TupleSections #-}

module Main where

import Numeric
import Data.Maybe
import Data.Map.Strict (Map (..))
import qualified Data.Map.Strict as Map
import Control.Applicative
import Control.Monad
import Graphs (Graph (..), findPath)
import Align (levenshteinDistance)

data Zip a = Zip ![a] ![a]
  deriving (Show, Functor, Eq, Ord) 

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

cursorAt :: (Int, Int) -> Board a -> Board a
cursorAt (i, j) b = rep i shiftR $ rep j (fmap shiftR) $ b
  where rep n = foldl (.) id . replicate n

flatten :: Board a -> [a]
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

movesA
  :: (Foldable t, Monad m, Alternative m) =>
     Board a -> t Move -> m (Board a)
movesA = foldM moveA

gameGraph :: Ord a => Graph (Board a)
gameGraph = Graph $ \b -> Map.fromList ((,1) <$> ([R,L,U,D] >>= moveA b))

distL1 b1 b2 = sum $ map abs $ zipWith (-) (flatten b1) (flatten b2)

task :: Board Int
task = cursorAt (2,0) $
  board [ [15, 14,  1,  6]
        , [ 9, 11,  4, 12]
        , [ 0, 10,  7,  3]
        , [13,  8,  5,  2] ]

goal :: Board Int
goal = cursorAt (3,3) $
  board [ [1,  2,  3,  4 ]
        , [5,  6,  7,  8 ]
        , [9,  10, 11, 12]
        , [13, 14, 15, 0 ] ]

prnt b = do
  mapM_ print $ toList $ toList <$> b
  putStr "\n"

s1 = foldl (flip move) goal [L,L,U,L,U,R,R,L,D,R,U,L,U,L,L,D,D,R,D,D,R,U,U,R,L,D,R,U,U,R,L]

main = do
  print (length p) 
  -- mapM_ prnt p
  where p = findPath gameGraph distL1 s1 goal

