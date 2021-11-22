{-# language DeriveFunctor #-}
import Control.Comonad

data Zip a = Zip { left :: [a]
                 , focus :: a
                 , right :: [a] }
  deriving (Show, Functor)

fromList :: [a] -> Zip a
fromList [] = Zip [] undefined []
fromList (h:t) = Zip [] h t

moveR (Zip l x (h:t)) = Zip (x:l) h t
moveR (Zip l x []) = Zip l x []

moveL (Zip (h:t) x r) = Zip t h (x:r)
moveL (Zip [] x r) = Zip [] x r

instance Comonad Zip where
  extract  = focus
  duplicate z = Zip l' z r'
    where
      l' = takeWhile (not . null . left) $ iterate moveL z
      r' = takeWhile (not . null . right) $ iterate moveR z

neigbours z = [extract (moveL z), extract (moveR z)]

cluster z = case extract z of
              0 -> 0
              c -> case neigbours z of
                [0,0] -> c
                [0,_] -> 0
                [_,0] -> 0
                [a,b] -> c + a + b
