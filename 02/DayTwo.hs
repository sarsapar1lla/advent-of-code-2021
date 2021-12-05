import System.IO
import Control.Monad


calculateHorizontalPosition :: [String] -> Int
calculateHorizontalPosition [] = 0
calculateHorizontalPosition (command:distance:rest)
    | command == "forward" = integerDistance + calculateHorizontalPosition rest
    | otherwise = calculateHorizontalPosition rest
    where integerDistance = read distance :: Int


calculateDepth :: [String] -> Int
calculateDepth [] = 0
calculateDepth (command:distance:rest)
    | command == "up" = (-integerDistance) + calculateDepth rest
    | command == "down" = integerDistance + calculateDepth rest
    | otherwise = calculateDepth rest
    where integerDistance = read distance :: Int


calculateFinalPosition :: [String] -> Int
calculateFinalPosition x = (calculateHorizontalPosition x) * (calculateDepth x)


main = do  
        contents <- readFile "data.txt"
        let list :: [String] = words $ contents
        print $ calculateHorizontalPosition list
        print $ calculateDepth list
        print $ calculateFinalPosition list
