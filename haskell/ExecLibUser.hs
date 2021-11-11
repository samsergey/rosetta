import ExecLib (hailstone)



main = length . hailstone <$> [1..1000]
