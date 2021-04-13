{-# language DeriveFunctor #-}

module Fefteen where

import Graph

data Zip a = Zip [a] [a]
  deriving (Show, Functor)

shift, unshift :: Zip a -> Zip a

shift (Zip l (x:r)) = Zip (x:l) r
shift z = z

unshift (Zip (x:l) r) = Zip l (x:r)
unshift z = z

fromList :: [a] -> Zip a
fromList = Zip []

type Board = Zip (Zip Int)

down, up, right, left :: Board -> Board

down = shift
up = unshift
right = fmap shift
left = fmap unshift

start :: Board
start = Zip [ Zip [9]  [11, 4, 12]
            , Zip [15] [14, 1, 6] ]
            [ Zip [0]  [10, 7, 3]
            , Zip [13]  [8, 5, 2] ]

 

