type Pt a = (a, a)

data Overlapping = Inside | Outside | Boundary
  deriving (Show, Eq)

data Triangle a = Triangle (Pt a) (Pt a) (Pt a)
  deriving Show

vertices (Triangle a b c) = [a, b, c]

toTriangle :: Num a => Triangle a -> Pt a -> (a, Pt a)
toTriangle t (x,y) = let
  [(x0,y0), (x1,y1), (x2,y2)] = vertices t
  s = x2*(y0-y1)+x0*(y1-y2)+x1*(-y0+y2)
  in  ( abs s
      , ( signum s * (x2*(-y+y0)+x0*(y-y2)+x*(-y0+y2))
        , signum s * (x1*(y-y0)+x*(y0-y1)+x0*(-y+y1))))

overlapping :: (Eq a, Ord a, Num a) =>
  Triangle a -> Pt a -> Overlapping
overlapping t p = case toTriangle t p of
  (s, (x, y))
    | s == 0 && (x == 0 || y == 0)     -> Boundary
    | s == 0                           -> Outside
    | x > 0 && y > 0 && y < s - x      -> Inside
    | (x <= s && x >= 0) &&
      (y <= s && y >= 0) &&
      (x == 0 || y == 0 || y == s - x) -> Boundary         
    | otherwise                        -> Outside
    
tests = let
  t1 = Triangle (2,0)  (-2,-2) (-1,2)
  bs = [(2,0), (-1,2), (-2,-2), (0,-1), (1/2,1), (-3/2,0)]
  is = [(0,0), (0,1), (-1,0), (-1,1), (-1,-1)]
  os = [(1,1), (-2,2), (100,100), (2.00000001, 0)]
  
  t2 = Triangle (1,2) (1,2) (-1,3)
  ps = [(1,2), (0,5/2), (0,2), (1,3)]

  in mapM_ print [ overlapping t1 <$> bs
                 , overlapping t1 <$> is
                 , overlapping t1 <$> os
                 , overlapping t2 <$> ps]

test2 = unlines
  [ [case overlapping t (i,j) of
        Inside -> '∗'
        Boundary -> '+'
        Outside -> '·'
    | i <- [-10..10] :: [Int] ]
  | j <- [-5..5] :: [Int] ]
  where t = Triangle (-8,-3) (8,1) (-1,4)
