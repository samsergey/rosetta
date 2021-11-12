{-# LANGUAGE FlexibleContexts #-}
import Data.List (unfoldr)

isEsthetic b = all ((== 1) . abs) . difs . digits b

difs lst = zipWith (-) lst (tail lst)

digits b = unfoldr f
  where
    f 0 = Nothing
    f n = let (q, r) = divMod n b in Just (r, q)

main = print $ filter (isEsthetic 10) [100000000..130000000]
