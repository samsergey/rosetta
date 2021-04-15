{-# language DeriveFunctor #-}
{-# language TupleSections #-}

module Main where

import Numeric
import Data.Bits (popCount, xor, (.&.))
import qualified Data.Bits as Bits
import Data.Maybe
import Data.Char (ord)
import Data.List (findIndex, unfoldr)
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

moves ms b = foldl (flip move) b ms

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

s1 = moves [L,L,U,L,U,R,R,L,D,R,U,L,U,L,L,D,D,R,D,D,R,U,U,R,L,D,R,U,U,R,L,L,D,R,U,U,R,L,D,R,U,U,R,L,D,R,U,U] goal


------------------------------------------------------------

newtype Board' = Board' ((Int,Int), Int)
  deriving (Eq, Ord)

instance Show Board' where
  showsPrec _ (Board' (_, n)) = showHex n

blank (Board' (p, _)) = p
board' (Board' (_, n)) = n

goal' = fromHex "123456789abcdef0"

fromHex :: String -> Board'
fromHex s = Board' ((i,j), fst $ head $ readHex s)
  where Just p = findIndex (== '0') s
        (i,j) = p `divMod` 4

{-# INLINE pos #-}
pos (i,j) = Bits.shiftL 1 $ 4*(15 - 4*i - j)

move' m b = fromMaybe b $ moveA' b m

moves' ms b = foldl (flip move') b ms

moveA' (Board' b) m = case m of
  L -> case b of
    ((i,0), n) -> empty
    ((i,j), n) -> let p = pos (i,j)
                      d = n `div` (Bits.shiftL p 4) `mod` 16
                  in pure $ Board' ((i,j-1), n + d*(p - Bits.shiftL p 4))
  R -> case b of
    ((i,3), n) -> empty
    ((i,j), n) -> let p = pos (i,j)
                      d = n `div` (Bits.shiftR p 4) `mod` 16
                  in pure $ Board' ((i,j+1), n + d*(p - Bits.shiftR p 4))
  U -> case b of
    ((0,j), n) -> empty
    ((i,j), n) -> let p = pos (i,j)
                      d = n `div` (Bits.shiftL p 16) `mod` 16
                  in pure $ Board' ((i-1,j), n + d*(p - Bits.shiftL p 16))
  D -> case b of
    ((3,j), n) -> empty
    ((i,j), n) -> let p = pos (i,j)
                      d = n `div` (Bits.shiftR p 16) `mod` 16
                  in pure $ Board' ((i+1,j), n + d*(p - Bits.shiftR p 16))
            
distL1' :: Board' -> Board' -> Int
distL1' b1 b2 = sum $ map abs $ zipWith (-) (toHex b1) (toHex b2)

{-# INLINE toHex #-}
toHex :: Board' -> [Int]
toHex (Board' (_, n)) = unfoldr f n
  where f 0 = Nothing
        f n = let (r, q) = divMod n 16
              in Just (q, r)

s1' = moves' [L,L,U,L,U,R,R,L,D,R,U,L,U,L,L,D,D,R,D,D,R,U,U,R,L,D,R,U,U,R,L,L,D,R,U,U,R,L,D,R,U,U,R,L,D,R,U,U] goal'

task' :: Board'
task' = fromHex "fe169b4c0a73d852"

tsk = moves' (map f "rrruldluuldrurdddluulurrrdlddruldluurddlulurruldrrdd") task'
  where f 'r' = R
        f 'l' = L
        f 'u' = U
        f 'd' = D

gameGraph' :: Graph Board'
gameGraph' = Graph $ \b -> Map.fromList ((,1) <$> ([R,L,U,D] >>= moveA' b))

------------------------------------------------------------

main = do
  print (length p) 
  -- mapM_ prnt p
  where p = findPath gameGraph' distL1' s1' goal'
