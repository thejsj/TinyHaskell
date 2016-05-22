

includes :: Eq a => a -> [a] -> Bool
includes _ [] = False
includes x l
  | any (==x) l = True
  | otherwise = False

find :: (a -> Bool) -> [a] -> Maybe a
find _ [] = Nothing
find f (x:xs)
  | f x = Just x
  | otherwise = find f xs

data Shape = Circle Float Float Float | Rectangle Float Float Float Float
  deriving(Show)
