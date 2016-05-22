import Data.List

wow :: (a -> b) -> [a] -> [b]
wow _ [] = []
wow f (x:xs) = f x : wow f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
  | p x       = x : filter' p xs
  | otherwise = filter p xs

wordNums :: String -> [(String, Int)]
wordNums a = map (\x -> (head x, length x)) $ (group . sort . words) a
