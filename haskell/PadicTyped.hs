{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Padic where

import Data.Type.Bool
import GHC.TypeLits
import Data.Constraint
import InfList (InfList(..), (+++))
import qualified InfList as Inf
import Data.List
import Data.Ratio
import Data.Word

------------------------------------------------------------

type family NonZeroNat (m :: Nat) :: Constraint where
  NonZeroNat 0 = TypeError ('Text "Zero base!")
  NonZeroNat m = ()

type family ValidBase (m :: Nat) :: Constraint where
  ValidBase m = (NonZeroNat m, KnownNat m)

type family Lg p n where
  Lg p 0 = 0
  Lg p n = Lg p (Div n p) + 1

type Base m = (ValidBase m, ValidBase (MaxBase m))

------------------------------------------------------------
  
class ValidBase p => Digital f p where
  base       :: Integral i => f p -> i
  digits'    :: f p -> InfList (Digit p)
  fromDigits':: InfList (Digit (MaxBase p)) -> f p

  digits :: Integral d => f p -> InfList d
  digits n = fromIntegral <$> digits' n

  fromDigits :: Integral d => InfList d -> f p
  fromDigits ds = fromDigits $ fromIntegral <$> ds

  firstDigit :: Integral a => f p -> a
  firstDigit = Inf.head . digits

class Fixed n where
  precision :: Integral i => n -> i

class Padic n where
  unit :: n -> n
  valuation :: n -> Int
  
------------------------------------------------------------

type N = Word64

type family MaxBase p where
  MaxBase p = p ^ (Lg p (2^64) - 1)

newtype Digit (m :: Nat) = Digit N
  deriving (Show, Num, Bounded, Eq, Real, Enum, Ord, Integral) via N

instance ValidBase p => Digital Digit p where
  base = fromIntegral . natVal

data Z (p :: Nat) where
   Z :: InfList (Digit (MaxBase p)) -> Z p

interior (Z ds) = ds

maxBase :: Integral i => i -> i
maxBase p = p ^ ilog p (maxBound :: N)

ilog b x = floor (logBase (fromIntegral b) (fromIntegral x))

demolish :: (Base p) => Digit (MaxBase p) -> [Digit p]
demolish n = res
  where
    b1 = base n
    b2 = base (head res)
    res
      | n == 0 = replicate (ilog b2 b1) 0
      | otherwise = toBase b2  n

toDigits :: ValidBase p => Integral i => i -> [Digit p] 
toDigits n = res
  where
    res = toBase (base (head res)) n 

toBase :: (Integral i, Integral d) => i -> i -> [d]
toBase b 0 = [0]
toBase b n = res 
  where
    res = unfoldr go n
    go 0 = Nothing
    go n = let (q, r) = quotRem n b
           in Just (fromIntegral r, q)

-- превращает целое число в p-адическое
toZ :: (Base p, Integral i) => i -> Z p
toZ n | n < 0 = - toZ (- n)
      | otherwise = res 
  where
    res = Z $ toDigits (fromIntegral n) +++ Inf.repeat 0

-- смена знака на противоположный
negZ :: (Base p) => Z p -> Z p
negZ (Z ds) = fromDigits $ go ds
  where go (0 ::: t) = 0 ::: go t
        go (h ::: t) = p - h ::: Inf.map (\x -> p - x - 1) t
        p = base (Inf.head ds)

-- выделяет из натурального числа перенос на следующий разряд
carry :: (ValidBase p, Integral n) => n -> (n, Digit p)
carry n =
  let d = fromIntegral (n `mod` base d)
  in (n `div` base d, d)

-- поразрядное сложение с переносом
--addZ :: (ValidBase p) => Z p -> Z p -> Z p
addZ a b = Inf.mapAccumL step 0 $ Inf.zip a b
  where
    step r (x, y) = carry (fromIntegral x + fromIntegral y + r)

mulZ a b = go b
  where
    go (b ::: bs) = addZ (go bs) `apTail` scaleZ b a
    apTail f (h ::: t) = h ::: f t

--scaleZ :: Base p => Mod p -> InfList (Mod p) -> InfList (Mod p)
scaleZ s a =
  Inf.mapAccumL (\r x -> carry (fromIntegral s * fromIntegral x + r)) 0 a


instance Base p => Digital Z p where
  digits' (Z ds) = Inf.concatMap demolish ds
  fromDigits' = Z
  base = fromIntegral . natVal

instance Base p => Num (Z p) where
  fromInteger = toZ
  Z a + Z b = Z $ addZ a b
  Z a * Z b = Z $ mulZ a b
  negate = negZ

newtype Z' (p :: Nat) (prec :: Nat) = Z' (Z p)
--  deriving (Digital) via (Z p)
