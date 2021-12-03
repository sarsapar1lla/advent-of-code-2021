import System.IO
import Control.Monad


toInt :: String -> Int
toInt = read

countIncreases :: [Int] -> Int
countIncreases [] = 0
countIncreases [curr] = 0
countIncreases (curr:next:rest)
        | next > curr = 1 + countIncreases ([next] ++ rest)
        | otherwise = countIncreases ([next] ++ rest)


main = do  
        contents <- readFile "data.txt"
        let list :: [Int] = map toInt . words $ contents
        print $ countIncreases list
