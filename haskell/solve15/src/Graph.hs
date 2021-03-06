module Graphs
  ( findPath
  , Graph (..) )

where

import Data.PriorityQueue.FingerTree (PQueue (..))
import qualified Data.PriorityQueue.FingerTree as Queue
import Data.Set (Set(..))
import qualified Data.Set as Set
import Data.Map.Strict (Map(..))
import qualified Data.Map.Strict as Map
import Data.List (unfoldr)
  
------------------------------------------------------------

newtype Graph n = Graph { links :: n -> Map n Int }

grid :: Int -> Int -> Graph (Int,Int)
grid a b = Graph $ \(x,y) ->
  let links = [((x+dx,y+dy), dx*dx+dy*dy)
              | dx <- [-1..1], dy <- [-1..1]
              , not (dx == 0 && dy == 0)
              , 0 <= x+dx && x+dx <= a
              , 0 <= y+dy && y+dy <= b]
  in Map.fromList links

withHole :: (Foldable t, Ord n) => Graph n -> t n -> Graph n
withHole (Graph g) ns = Graph $ \x ->
  if x `elem` ns
  then Map.empty
  else foldr Map.delete (g x) ns 

distL2 (x,y) (a,b) = (x-a)^2 + (y-b)^2
distL1 (x,y) (a,b) = max (abs $ x-a) (abs $ y-b)

------------------------------------------------------------

get :: (Ord k, Bounded a) => Map k a -> k -> a 
get m x = Map.findWithDefault maxBound x m

set :: Ord k => Map k a -> k -> a -> Map k a
set m k x = Map.insert k x m 

data AstarData n = SetData { cameFrom :: Map n n
                           , gScore   :: Map n Int
                           , openSet  :: PQueue Int n
                           , visited  :: Set n }

findPath
  :: Ord n => Graph n -> (n -> n -> Int) -> n -> n -> [n]
findPath (Graph links) metric start goal = loop a0
  where
    a0 = SetData
         { cameFrom = mempty
         , gScore   = Map.singleton start 0
         , openSet  = Queue.singleton (h start) start
         , visited  = mempty }
    h = metric goal
    dist = get . links

    loop a = case Queue.minView (openSet a) of
      Nothing -> []
      Just (current, q') -> if current == goal
                            then getPath (cameFrom a)
                            else loop a'
        where
          a' = Map.foldlWithKey go a { openSet = q'
                                     , visited = Set.insert current (visited a) } neighbours
          neighbours = links current
          go a n _ =
            let g = get $ gScore a
                trial_Score = g current + dist current n
            in if trial_Score >= g n && False -- Set.member n (visited a)
               then a 
               else SetData
                    ( set (cameFrom a) n current )
                    ( set (gScore a) n trial_Score )
                    ( Queue.insert (trial_Score + h n) n (openSet a) )
                    ( visited a )

    getPath m = reverse $ goal : unfoldr go goal
      where go n = (\x -> (x,x)) <$> Map.lookup n m

------------------------------------------------------------

task = let
  g = grid 9 9 `withHole` wall
  wall = [ (2,4),(2,5),(2,6),(3,6)
         , (4,6),(5,6),(5,5),(5,4)
         , (5,3),(5,2),(3,2),(4,2) ]
  path = findPath g distL1 (1,1) (7,7)
  picture = [ [ case (i,j) of
                  p | p `elem` path -> '*'
                    | p `elem` wall -> '#'
                    | otherwise     -> ' '
              | i <- [0..8] ]
            | j <- [0..8] ]
  in do 
    print path
    mapM_ putStrLn picture
    putStrLn $ "Path score: " <> show (length path) 
    
    

