{-# language DeriveFunctor #-}
{-# language TupleSections #-}

module Main where

import Game
import Graph

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


s1 = foldl (flip move) goal [L,L,U,L,U,R,R,L,D,R,U,L,U,L,L,D,D,R,D,D,R,U,U,R,L,D,R,U,U,R,L,D]

main = do
  print (length p)
  mapM_ prnt p
  where p = findPath gameGraph distL1 task goal
