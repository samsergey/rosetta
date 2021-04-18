import Data.List

sod = sum . digits

selfF n = n + sod n

digits = unfoldr f
  where f 0 = Nothing
        f n = let (q,d) = divMod n 10 in Just (d,q)

selfs = go [1..] $ selfF <$> [1..]
  where
    go (x:xs) (y:ys)
      | x > y = go (x:xs) ys
      | x `notElem` take 81 (y:ys) = x : go xs (y:ys)
      | otherwise = go xs (y:ys)

main = do
  print $ take 50 selfs
  print $ selfs !! (10^5-1) 
--  print $ filter isSelf [1..] !! (10^5-1)

isSelf n | n < 10 = odd n 
isSelf n = all (\i -> selfF (n - c - 9*i) /= n) [0..length ds]
  where c = let d = 1 + (sum ds - 1) `mod` 9
            in if even d then d `div` 2 else (d + 9) `div` 2
        ds = digits n
