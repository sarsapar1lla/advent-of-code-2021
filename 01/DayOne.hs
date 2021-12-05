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


countIncreasesSliding :: [Int] -> Int
countIncreasesSliding [] = 0
countIncreasesSliding [a] = 0
countIncreasesSliding [a, b] = 0
countIncreasesSliding [a, b, c] = 0
countIncreasesSliding (a:b:c:d:rest)
        | b + c + d > a + b + c = 1 + countIncreasesSliding ([b] ++ [c] ++ [d] ++ rest)
        | otherwise = countIncreasesSliding ([b] ++ [c] ++ [d] ++ rest)


main = do  
        contents <- readFile "data.txt"
        let list :: [Int] = map toInt . words $ contents
        print $ countIncreases list
        print $ countIncreasesSliding list
