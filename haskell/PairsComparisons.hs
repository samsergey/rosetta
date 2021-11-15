newtype TotalOrder a = TotalOrder {relation :: [(a,a)]}
  deriving Show

unordered :: Eq a => [a] -> TotalOrder a -> [a]
unordered set rel = set

isOrdered (TotalOrder rel) (a,b) = (a,b) `elem` rel

-- forceTransitivity :: Eq a => [a] -> TotalOrder a -> TotalOrder a
addRelation r (TotalOrder []) = TotalOrder [r]
addRelation (a,b) ord = TotalOrder $
  mconcat [[(a,x), (a,y)] | (x, y) <- relation ord
                          , isOrdered ord (b, x)
                          , isOrdered ord (b, y)] <>
  relation ord
