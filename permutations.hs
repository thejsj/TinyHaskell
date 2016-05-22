permutationsForString :: [String] -> Int -> String -> [String]
permutationsForString l n s
  | length s > n = []
  | otherwise = concat $ [s] : map (\x -> permutationsForString l n (s ++ x)) l

main = do
  putStrLn "String length?"
  n' <- getLine
  putStrLn "Strings?"
  strs' <- getLine
  let n = read n' :: Int
  let strs = read strs' :: [String]
  print (permutationsForString strs n "")
  putStrLn "Done"

