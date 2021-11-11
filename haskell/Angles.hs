{-# Language GeneralizedNewtypeDeriving #-}
{-# Language MultiParamTypeClasses #-}
{-# Language UndecidableSuperClasses #-}
{-# Language TypeApplications #-}
{-# Language FlexibleInstances, RankNTypes,MonoLocalBinds  #-}
  
import Text.Printf

------------------------------------------------------------
-- Common class for all angular values.
-- In order to define new value one needs to instatiate it,
-- providing all information in one place.

class (Num a, Fractional a, RealFrac a) => Angle a where
  -- value of the whole turn
  turn :: a
  mkAngle :: Double -> a
  value :: a -> Double

-- smart constructor with normalizer
angle :: Angle a => Double -> a
angle x = mkAngle x `modulo` turn
  where
    modulo x r | x == r = r
               | x < 0 = signum x * abs x `modulo` r
               | x >= 0 = x - fromInteger (floor (x / r)) * r 

-- Isomorphims between all angular types are defined
-- via numbers of turns.

class Iso b a => Iso a b where 
  from :: a -> b

to :: forall b a. Iso a b => a -> b
to = from

instance (Angle a, Angle b) => Iso a b where
  from = fromTurn . toTurn

fromTurn :: Angle a => Double -> a
fromTurn t = angle t * turn

toTurn :: Angle a => a -> Double
toTurn a = value $ a / turn

------------------------------------------------------------
-- radians

newtype Rad = Rad Double
  deriving (Eq, Ord, Num, Real, Fractional, RealFrac, Floating)

instance Show Rad where
  show (Rad r) = printf "∠ %.4f" r 

instance Angle Rad where
  turn = Rad 2*pi
  mkAngle = Rad
  value (Rad r) = r

------------------------------------------------------------
-- degrees

newtype Deg = Deg Double
  deriving (Eq, Ord, Num, Real, Fractional, RealFrac, Floating)

instance Show Deg where
  show (Deg d) =  printf "%.4g º" d

instance Angle Deg where
  turn = Deg 360
  mkAngle = Deg
  value (Deg d) = d

------------------------------------------------------------
-- gons (grads)

newtype Gon = Gon Double
  deriving (Eq, Ord, Num, Real, Fractional, RealFrac, Floating)

instance Show Gon where
  show = printf "%.4g gon" . value

instance Angle Gon where
  turn = Gon 400
  mkAngle = Gon
  value (Gon g) = g
  
------------------------------------------------------------
-- mils

newtype Mil = Mil Double
  deriving (Eq, Ord, Num, Real, Fractional, RealFrac, Floating)

instance Show Mil where
  show (Mil m) =  printf "%.4g mil" m

instance Angle Mil where
  turn = Mil 6400
  mkAngle = Mil
  value (Mil m) = m

------------------------------------------------------------
-- task

main = do
  let xs = [-2, -1, 0, 1, 2, 6.2831853,
            16, 57.2957795, 359, 399, 6399, 1000000]
           
  putStrLn "converting from radians"
  print $ to @Rad . angle @Rad <$> xs
  print $ to @Deg . angle @Rad <$> xs
  print $ to @Gon . angle @Rad <$> xs
  print $ to @Mil . angle @Rad <$> xs
  putStrLn "\nconverting from degrees"
  print $ to @Rad . angle @Deg <$> xs
  print $ to @Deg . angle @Deg <$> xs
  print $ to @Gon . angle @Deg <$> xs
  print $ to @Mil . angle @Deg <$> xs
  putStrLn "\nconverting from grads"
  print $ to @Rad . angle @Gon <$> xs
  print $ to @Deg . angle @Gon <$> xs
  print $ to @Gon . angle @Gon <$> xs
  print $ to @Mil . angle @Gon <$> xs
  putStrLn "\nconverting from mils"
  print $ to @Rad . angle @Mil <$> xs
  print $ to @Deg . angle @Mil <$> xs
  print $ to @Gon . angle @Mil <$> xs
  print $ to @Mil . angle @Mil <$> xs


  
  

