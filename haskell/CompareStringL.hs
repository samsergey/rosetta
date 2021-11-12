import qualified Data.Text as T

task s1 s2 = do
  let strs = if length s1 > length s2 then [s1, s2] else [s2, s1]
  mapM_ (\s -> putStrLn $ show (length s) ++ "\t" ++ show s) strs

task1' s1 s2 = do
  let strs = if T.length s1 > T.length s2 then [s1,s2] else [s2,s1]
  mapM_ (\s -> putStrLn $ show (T.length s) ++ "\t" ++ show s) strs

