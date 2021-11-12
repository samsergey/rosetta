import Data.Maybe
import Data.List
import Control.Monad.State
import qualified Data.Map as M
import Text.Printf
<<<<<<< HEAD
 
------------------------------------------------------------
-- Primitive graph representation
 
type Node = Int
type Color = Int
type Graph = M.Map Node [Node]
 
nodes :: Graph -> [Node]
nodes = M.keys
 
adjacentNodes :: Graph -> Node -> [Node]
adjacentNodes g n = fromMaybe [] $ M.lookup n g 
 
degree :: Graph -> Node -> Int 
degree g = length . adjacentNodes g
 
=======

------------------------------------------------------------
-- Primitive graph representation

type Node = Int
type Color = Int
type Graph = M.Map Node [Node]

nodes :: Graph -> [Node]
nodes = M.keys

adjacentNodes :: Graph -> Node -> [Node]
adjacentNodes g n = fromMaybe [] $ M.lookup n g 

degree :: Graph -> Node -> Int 
degree g = length . adjacentNodes g

>>>>>>> 231348938c4036a6d68d0d625c9b3c476363cc15
fromList :: [(Node, [Node])] -> Graph
fromList  = foldr add M.empty
  where
    add (a, bs) g = foldr (join [a]) (join bs a g) bs
    join = flip (M.insertWith (++))
<<<<<<< HEAD
 
=======

>>>>>>> 231348938c4036a6d68d0d625c9b3c476363cc15
readGraph :: String -> Graph
readGraph = fromList . map interprete . words
  where
    interprete s = case span (/= '-') s of
      (a, "") -> (read a, [])
      (a, '-':b) -> (read a, [read b])
<<<<<<< HEAD
 
------------------------------------------------------------
-- Graph coloring functions
 
uncolored :: Node -> State [(Node, Color)] Bool
uncolored n = isNothing <$> colorOf n
 
colorOf :: Node -> State [(Node, Color)] (Maybe Color)
colorOf n = gets (lookup n)

  
=======

------------------------------------------------------------
-- Graph coloring functions

uncolored :: Node -> State [(Node, Color)] Bool
uncolored n = isNothing <$> colorOf n

colorOf :: Node -> State [(Node, Color)] (Maybe Color)
colorOf n = gets (lookup n)

>>>>>>> 231348938c4036a6d68d0d625c9b3c476363cc15
greedyColoring :: Graph -> [(Node, Color)]
greedyColoring g = mapM_ go (nodes g) `execState` []
  where
    go n = do
      c <- colorOf n
      when (isNothing c) $ do
        adjacentColors <- nub . catMaybes <$> mapM colorOf (adjacentNodes g n)
        let newColor = head $ filter (`notElem` adjacentColors) [1..]
        modify ((n, newColor) :)
      filterM uncolored (adjacentNodes g n) >>= mapM_ go

<<<<<<< HEAD

=======
>>>>>>> 231348938c4036a6d68d0d625c9b3c476363cc15
wpColoring :: Graph -> [(Node, Color)]
wpColoring g = go [1..] nodesList `execState` []
  where
    nodesList = sortOn (negate . degree g) (nodes g)
<<<<<<< HEAD
 
    go _ [] = pure ()
    go (c:cs) ns = do 
      mark c ns 
      filterM uncolored ns >>= go cs
 
    mark c [] = pure () :: State [(Node, Color)] ()
    mark c (n:ns) = do 
      modify ((n, c) :) 
      mark c (filter (`notElem` adjacentNodes g n) ns)

------------------------------------------------------------

ex1 = "0-1 1-2 2-0 3"      
ex2 = "1-6 1-7 1-8 2-5 2-7 2-8 3-5 3-6 3-8 4-5 4-6 4-7"      
ex3 = "1-4 1-6 1-8 3-2 3-6 3-8 5-2 5-4 5-8 7-2 7-4 7-6"
ex4 = "1-6 7-1 8-1 5-2 2-7 2-8 3-5 6-3 3-8 4-5 4-6 4-7"
ex5 = "1-2 1-3 1-5 1-6 2-4 2-5 2-6 3-4 3-5 4-5 4-6 5-6"

task :: String -> IO ()
task s = do
  let g = readGraph s
  let coloring = sortOn fst $ greedyColoring g
  let color n = fromJust $ lookup n coloring
  let cs n = show $ color <$> adjacentNodes g n
  print s
  putStrLn $ "nodes:\t" ++ show (nodes g)
  putStrLn $ "colors:\t" ++ show (nub $ snd <$> coloring)
  putStrLn "node\tcolor\tadjacent colors"
  mapM_ (\(n, c) -> printf "%d\t%d\t%s\n" n c (cs n)) coloring
  putStrLn ""

main = mapM_ task [ex1,ex2,ex3,ex4,ex5]
=======

    go _ [] = pure ()
    go (c:cs) ns = do
      mark c ns
      filterM uncolored ns >>= go cs

    mark c [] = pure () :: State [(Node, Color)] ()
    mark c (n:ns)  = do
      modify ((n, c) :)
      mark c (filter (`notElem` adjacentNodes g n) ns)


ex1 = "0-1 1-2 2-0 3"
ex2 = "1-6 1-7 1-8 2-5 2-7 2-8 3-5 3-6 3-8 4-5 4-6 4-7"
ex3 = "1-4 1-6 1-8 3-2 3-6 3-8 5-2 5-4 5-8 7-2 7-4 7-6"
ex4 = "1-6 7-1 8-1 5-2 2-7 2-8 3-5 6-3 3-8 4-5 4-6 4-7"

task method ex =
  let g = readGraph ex
      cs = sortOn fst $ method g
      color n = fromJust $ lookup n cs
      ns = nodes g
      mkLine n = printf "%d\t%d\t%s\n" n (color n) (show (color <$> adjacentNodes g n))
  in do
    print ex
    putStrLn $ "nodes:\t" ++ show ns
    putStrLn $ "colors:\t" ++ show (nub (snd <$> cs))
    putStrLn "node\tcolor\tadjacent colors"
    mapM_ mkLine ns
    putStrLn ""
  

main = pure ()

--mapM_ (task greedyColoring) [ex1,ex2,ex3,ex4]
--mapM_ (task wpColoring) [ex1,ex2,ex3,ex4]
  
>>>>>>> 231348938c4036a6d68d0d625c9b3c476363cc15
