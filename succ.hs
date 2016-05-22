succA :: Enum a => a -> a
succA x = succ x

succN :: Enum a => Int -> a -> a
succN 0 b = b
succN n b = succN (n - 1) $ succ b

main = print $ show (succN 100 1)
