{-# LANGUAGE KindSignatures, DataKinds  #-}
module Padic where

import Data.Ratio
import Data.List (genericLength, find)
import GHC.TypeLits 



data Padic (n :: Nat) = Null
                      | Padic { unit :: [Int]
                              , val :: Int }
