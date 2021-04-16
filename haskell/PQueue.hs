{-# language DeriveFoldable #-}

module PQueue where

-- The simplest FIFO priority queue is implemented after Sleator and Tarjan in Louis Wasserman's ("Playing with Priority Queues")[https://themonadreader.files.wordpress.com/2010/05/issue16.pdf].

data PQueue k a = EmptyQueue
                | Node !(k, a) !(PQueue k a) !(PQueue k a)
  deriving (Show, Foldable)

instance (Ord k, Ord a) => Semigroup (PQueue k a) where
  h1@(Node (w1, x1) l1 r1) <> h2@(Node (w2, x2) l2 r2) 
    | w1 < w2   = Node (w1, x1) (h2 <> r1) l1 
    | otherwise = Node (w2, x2) (h1 <> r2) l2
  EmptyQueue <> h = h
  h <> EmptyQueue = h

singleton :: (Ord k, Ord a) => k -> a -> PQueue k a
singleton w x = Node (w, x) EmptyQueue EmptyQueue

insert :: (Ord k, Ord a) => k -> a -> PQueue k a -> PQueue k a
insert w x q = if x `notElem` q
               then singleton w x <> q
               else q

minView :: (Ord k, Ord a) => PQueue k a -> Maybe (a, PQueue k a)
minView q = case q of
              EmptyQueue -> Nothing
              Node (_, x) l r -> Just (x, l <> r)
