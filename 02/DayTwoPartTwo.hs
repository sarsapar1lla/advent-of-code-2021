import System.IO
import Control.Monad


calculateHorizontalPosition :: [String] -> Int
calculateHorizontalPosition [] = 0
calculateHorizontalPosition (command:distance:rest)
    | command == "forward" = integerDistance + calculateHorizontalPosition rest
    | otherwise = calculateHorizontalPosition rest
    where integerDistance = read distance :: Int


calculateDepth :: [String] -> Int -> Int
calculateDepth [] _ = 0
calculateDepth (command:distance:rest) aim
    | command == "forward" = aim * integerDistance + calculateDepth rest aim
    | command == "up" = calculateDepth rest (aim - integerDistance)
    | command == "down" = calculateDepth rest (aim + integerDistance)
    | otherwise = calculateDepth rest aim
    where integerDistance = read distance :: Int


calculateFinalPosition :: [String] -> Int
calculateFinalPosition x = (calculateHorizontalPosition x) * (calculateDepth x 0)


main = do  
        contents <- readFile "data.txt"
        let list :: [String] = words $ contents
        print $ calculateHorizontalPosition list
        print $ calculateDepth list 0
        print $ calculateFinalPosition list
