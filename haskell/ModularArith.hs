{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE KindSignatures, TypeApplications #-}
{-# language DataKinds #-}

import Data.Ratio
import Data.Maybe (fromMaybe)
import Data.List
import Control.Monad (guard)
import GHC.TypeLits hiding (Mod(..))


mBase :: (KnownNat m, Integral i) => f m -> i
mBase = fromIntegral . natVal

modulo a x = a `mod` mBase x

------------------------------------------------------------

newtype Z (n :: Nat) = Z Int
  deriving Show via Int

instance KnownNat m => Num (Z m) where
  fromInteger n =
    let res = Z (fromInteger n `modulo` res)
    in res
      
  x@(Z a) + Z b = Z $ (a + b) `modulo` x
  x@(Z a) * Z b = Z $ (a * b) `modulo` x
  negate x@(Z a) = Z $ mBase x - a
  abs a = a
  signum a = undefined

instance KnownNat m => Eq (Z m) where
  x@(Z a) == Z b = (a - b) `modulo` x == 0

instance KnownNat m => Fractional (Z m) where
  fromRational r = let
    (a,b) = (numerator r, denominator r)
    in fromInteger a / fromInteger b

  recip x@(Z 1) = Z 1
  recip x@(Z a)
    | gcd p a == 1 = Z $ go 0 1 p a
    | otherwise = error $
      show x ++ "is not invertible mBase " ++ show p
    where
      p = mBase x
      go t _ _ 0 = t `mod` p
      go t nt r nr =
        let q = r `div` nr
        in go nt (t - q*nt) nr (r - q*nr)

------------------------------------------------------------

newtype ZP (n :: Nat) = ZP [Int]
  deriving Show via [Int]

instance KnownNat m => Num (ZP m) where

  fromInteger 0 = ZP [0]
  fromInteger n = res
    where
      m = mBase res
      res = ZP $ unfoldr go n
      go 0 = Nothing
      go n = let (q,r) = n `divMod` m
             in Just (fromIntegral r, q)
    
  x@(ZP a) + ZP b = ZP $ go 0 a b
    where
      p = mBase x
      go 0 [] ys = ys
      go 0 xs [] = xs
      go s [] ys = go 0 [s] ys
      go s xs [] = go 0 xs [s]
      go s (x:xs) (y:ys) =
        let (q, r) = (x + y + s) `divMod` p
        in r : go q xs ys

  x@(ZP a) * ZP b = ZP $ go b
    where
      p = mBase x
      go [] = []
      go (b:bs) =
        let c:cs = scaleZPP p b a
        in c : addZPP p (go bs) cs

  negate (ZP []) = ZP []
  negate x@(ZP (a:as)) = ZP $ (p-a) : ((p - 1 -) <$> as)
    where
      p = mBase x
    


------------------------------------------------------------


    
-- Reciprocal of a number mBase p (extended Euclidean algorythm).
-- For non-prime p returns Nothing non-inverible element of the ring.
recipZ :: Integral i => i -> i -> Maybe i
recipZ p 1 = Just 1
recipZ p a | gcd p a == 1 = Just $ go 0 1 p a
             | otherwise = Nothing
  where
    go t _ _ 0 = t `mod` p
    go t nt r nr =
      let q = r `div` nr
      in go nt (t - q*nt) nr (r - q*nr)

-- Addition of two sequences mBase p
addZP p = go 0
  where
    go 0 [] ys = ys
    go 0 xs [] = xs
    go s [] ys = go 0 [s] ys
    go s xs [] = go 0 xs [s]
    go s (x:xs) (y:ys) =
      let (q, r) = (x + y + s) `divMod` p
      in r : go q xs ys

-- Multiplication of sequence by a number mBase p
scaleZP p 0 _ = repeat 0
scaleZP p 1 b = b
scaleZP p a b = go 0 b
  where
    go s [] = [s]
    go s (b:bs) =
      let (q, r) = (a * b + s) `divMod` p
      in r : go q bs

-- Multiplication of two sequences mBase p
mulZ p as = go
  where
    go [] = []
    go (b:bs) =
      let c:cs = scaleZP p b as
      in c : addZP p (go bs) cs

longDivZ p a (b:bs) = case recipZ p b of
  Nothing -> error $
    show b ++ " is not invertible mBase " ++ show p
  Just r -> go a
    where
      go [] = []
      go (0:xs) = 0 : go xs
      go (x:xs) =
        let m = (x*r) `mod` p
            _:zs = subZ p (x:xs) (scaleZP p m (b:bs))
        in m : go (tail zs)
        
subZ p a (b:bs) = addZP p a $ (p-b) : ((p - 1 -) <$> bs)
