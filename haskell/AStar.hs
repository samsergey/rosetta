module AStar where

import PQueue
import Data.Map (Map(..))
import qualified Data.Map as Map
import Data.List (unfoldr)
 
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


get :: (Ord k, Bounded a) => Map k a -> k -> a
get m x = Map.findWithDefault maxBound x m
 
set :: Ord k => Map k a -> k -> a -> Map k a
set m k x = Map.insert k x m
 
data AstarData n = SetData { cameFrom :: Map n n
                           , gScore   :: Map n Int
                           , openSet  :: PQueue n }
 
findPath
  :: Ord n => Graph n -> (n -> n -> Int) -> n -> n -> [n]
findPath (Graph links) metric start goal = loop a0
  where
    a0 = SetData
         { cameFrom = mempty
         , gScore   = Map.singleton start 0
         , openSet  = entry start (h start) }
    h = metric goal
    dist = get . links
 
    loop a = case deque (openSet a) of
      Nothing -> []
      Just (current, q') -> if current == goal
                            then getPath (cameFrom a)
                            else loop a'
        where
          a' = Map.foldlWithKey go a { openSet = q' } neighbours
          neighbours = links current
          go a n _ =
            let g = get $ gScore a
                trial_Score = g current + dist current n
            in if trial_Score >= g n
               then a
               else SetData
                    ( set (cameFrom a) n current )
                    ( set (gScore a) n trial_Score )
                    ( openSet a `enque` n $ trial_Score + h n )
 
    getPath m = reverse $ goal : unfoldr go goal
      where go n = (\x -> (x,x)) <$> Map.lookup n m
