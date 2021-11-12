{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
  
import Text.Printf

------------------------------------------------------------
-- Common class for all angular units, which are represented
-- as distinct types. In order to define a new unit one needs
-- to instatiate it, providing all information in one place.
 
class (Num a, Fractional a, RealFrac a) => Angle a where
  fullTurn :: a -- value of the whole turn
  mkAngle :: Double -> a
  value :: a -> Double
  fromTurn :: Double -> a
  toTurn :: a -> Double
  normalize :: a -> a

  -- conversion of angles to rotations in linear case
  fromTurn t = angle t * fullTurn
  toTurn a = value $ a / fullTurn

  -- normalizer for linear angular unit
  normalize a = a `modulo` fullTurn
    where
      modulo x r | x == r = r
                 | x < 0 = signum x * abs x `modulo` r
                 | x >= 0 = x - fromInteger (floor (x / r)) * r 

-- smart constructor
angle :: Angle a => Double -> a
angle = normalize . mkAngle

-- Isomorphims between all angular types are defined
-- via numbers of fullTurns.
-- Two morphisms differ only in the order of type application.

from :: forall a b. (Angle a, Angle b) => a -> b
from = fromTurn . toTurn

to :: forall b a. (Angle a, Angle b) => a -> b
to = fromTurn . toTurn

 
------------------------------------------------------------
-- Instances of angular units

-- radians
newtype Rad = Rad Double
  deriving (Eq, Ord, Num, Real, Fractional, RealFrac, Floating)
 
instance Show Rad where
  show (Rad 0) = printf "∠0" 
  show (Rad r) = printf "∠%.3f" r 
 
instance Angle Rad where
  fullTurn = Rad 2*pi
  mkAngle = Rad
  value (Rad r) = r
 
-- degrees
newtype Deg = Deg Double
  deriving (Eq, Ord, Num, Real, Fractional, RealFrac, Floating)
 
instance Show Deg where
  show (Deg 0) = printf "0°"
  show (Deg d) =  printf "%.3g°" d
 
instance Angle Deg where
  fullTurn = Deg 360
  mkAngle = Deg
  value (Deg d) = d
 
-- grads 
newtype Grad = Grad Double
  deriving (Eq, Ord, Num, Real, Fractional, RealFrac, Floating)
 
instance Show Grad where
  show (Grad 0) = printf "0g"
  show (Grad g) = printf "%.3gg" g
 
instance Angle Grad where
  fullTurn = Grad 400
  mkAngle = Grad
  value (Grad g) = g
  
-- mils
newtype Mil = Mil Double
  deriving (Eq, Ord, Num, Real, Fractional, RealFrac, Floating)
 
instance Show Mil where
  show (Mil 0) = printf "0m"
  show (Mil m) =  printf "%.3gm" m
 
instance Angle Mil where
  fullTurn = Mil 6400
  mkAngle = Mil
  value (Mil m) = m


-- example of non-linear angular unit 
newtype Slope = Slope Double
  deriving (Eq, Ord, Num, Real, Fractional, RealFrac, Floating)
 
instance Show Slope where
  show (Slope 0) = printf "0%"
  show (Slope m) = printf "%.g" (m * 100) ++ "%"
 
instance Angle Slope where
  fullTurn = undefined
  mkAngle = Slope
  value (Slope t) = t
  toTurn   = toTurn @Rad . angle . atan . value
  fromTurn = angle . tan . value . fromTurn @Rad
  normalize = id

------------------------------------------------------------
-- task
  
main = do
  let xs = [-2, -1, 0, 1, 2, 6.2831853,
            16, 57.2957795, 359, 399, 6399, 1000000]

  -- using `to` and `angle` with type application
  putStrLn "converting to radians"
  print $ to @Rad . angle @Rad <$> xs
  print $ to @Rad . angle @Deg <$> xs
  print $ to @Rad . angle @Grad <$> xs
  print $ to @Rad . angle @Mil <$> xs
  print $ to @Rad . angle @Slope <$> xs

  -- using `from` with type application
  putStrLn "\nconverting to degrees"
  print $ from @Rad @Deg . angle <$> xs
  print $ from @Deg @Deg . angle <$> xs
  print $ from @Grad @Deg . angle <$> xs
  print $ from @Mil @Deg . angle <$> xs
  print $ from @Slope @Deg . angle <$> xs

  -- using normalization for each unit
  putStrLn "\nconverting to grads"
  print $ to @Grad . normalize . Rad <$> xs
  print $ to @Grad . normalize . Deg <$> xs
  print $ to @Grad . normalize . Grad <$> xs
  print $ to @Grad . normalize . Mil <$> xs
  print $ to @Grad . normalize . Slope <$> xs

  -- using implicit type annotation
  putStrLn "\nconverting to mils"
  print $ (from :: Rad -> Mil) . angle <$> xs
  print $ (from :: Deg -> Mil) . angle <$> xs
  print $ (from :: Grad -> Mil) . angle <$> xs
  print $ (from :: Mil -> Mil) . angle <$> xs
  print $ (from :: Slope -> Mil) . angle <$> xs
