import Data.Monoid
import Control.Monad (guard)

import Data.Monoid
 
data Elliptic = Elliptic Double Double | Zero
   deriving Show
 
instance Eq Elliptic where
  p == q = dist p q < 1e-14
    where
      dist Zero Zero = 0
      dist Zero p = 1/0
      dist p Zero = 1/0
      dist (Elliptic x1 y1) (Elliptic x2 y2) = (x2-x1)^2 + (y2-y1)^2
 
inv Zero = Zero
inv (Elliptic x y) = Elliptic x (-y)


instance Semigroup Elliptic where
  Zero <> p = p
  p <> Zero = p
  p@(Elliptic x1 y1) <> q@(Elliptic x2 y2)
    | p == inv q = Zero
    | p == q     = mkElliptic $ 3*x1^2/(2*y1)
    | otherwise  = mkElliptic $ (y2 - y1)/(x2 - x1)
    where
      mkElliptic l = let x = l^2 - x1 - x2
                         y = l*(x1 - x) - y1
                     in Elliptic x y


instance Monoid Elliptic where
  mempty = Zero
 

ellipticX b y = Elliptic (qroot (y^2 - b)) y
  where qroot x = signum x * abs x ** (1/3)

main = pure ()
