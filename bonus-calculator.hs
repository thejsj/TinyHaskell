import Data.List.Split
import Data.Char
import Data.Maybe
import Text.Read

converToFloat :: String -> Maybe Float
converToFloat x = readMaybe x :: Maybe Float

notNull = not . null

convertStringsToFloats :: [String] -> [Float]
convertStringsToFloats x = map fromJust $ filter notNull $ map converToFloat x

splitIntoNumbers :: String -> [Float]
splitIntoNumbers x = convertStringsToFloats $ splitOn "," x

assignBonus :: Float -> Float
assignBonus x
  | x <= 10000.0 = x * 0.03
  | x <= 15000.0 = x * 0.06
  | x <= 25000.0 = x * 0.09
  | otherwise    = x * 0.15

getBonusesForEmployess = (map assignBonus) . splitIntoNumbers

main = do
  print "Sales?:"
  input <- getLine
  print "Result:"
  print $ getBonusesForEmployess input
