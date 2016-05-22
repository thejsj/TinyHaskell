import qualified Data.Text as T
import Control.Lens
import System.Environment (getArgs)
import Data.List.Split (splitOn)
import Data.List (intersperse, intercalate)
import Data.Text (strip)
import Text.Read (readMaybe)
import Data.String.Utils (replace)

-- Constants
fileName = "./todos.md"
todoStart = "- [ ] "

-- Utils
getTodos :: IO [String]
getTodos = do
  contents <- readFile fileName
  let todos = filter ((/="") . T.unpack . T.strip . T.pack) $ splitOn "\n" contents
  return todos

-- Why do I even need this function????
deleteAt :: Int -> [a] -> [a]
deleteAt 0 (x:xs) = xs
deleteAt n (x:xs) | n >= 0 = x : (deleteAt (n-1) xs)
deleteAt _ _ = error "index out of range"

-- Main Methods
add :: Maybe String -> IO ()
add (Just a) = appendFile fileName (todoStart ++ a ++ "\n")
add Nothing = print "No string passed to `add`"

delete :: IO ()
delete = do
  todos <- getTodos
  let todosNoStart = map (replace todoStart "") todos
  let todosWithIndex = zipWith (++) (map ((++ " - ") . show) [0..]) todosNoStart

  mapM_ putStrLn todosWithIndex
  putStrLn "Select TODO to delete:"
  deleteIndex <- getLine

  let i = read deleteIndex :: Int
  let newTodos' = deleteAt i todosNoStart
  let newTodos = map (\x -> (todoStart ++ x)) newTodos'
  writeFile fileName (intercalate "\n" newTodos)

list :: IO ()
list = do
  todos <- getTodos
  mapM_ putStrLn todos

notFound :: IO ()
notFound = putStr "Sorry, your command could not be found"

-- Main
dispatch :: Maybe String -> Maybe String -> IO ()
dispatch (Just "add") a = add a
dispatch (Just "delete") _  = delete
dispatch (Just "list") _ = list
dispatch _ _ = notFound

main = do
  values <- getArgs
  let command = values ^? element 0
  let todo = values ^? element 1
  dispatch command todo
