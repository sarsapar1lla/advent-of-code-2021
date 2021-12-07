import System.IO
import Control.Monad
import Data.List (sort)


prepareData :: String -> [Int]
prepareData = (map read) . words . (map repl)
    where repl ',' = ' '
          repl c = c


calculateCostOfPosition :: [Int] -> Int -> Int
calculateCostOfPosition [] _ = 0
calculateCostOfPosition crabs@(c:cs) position = 
    let
        cost = abs (c - position)
    in  cost + calculateCostOfPosition cs position


calculateCostOfPosition' :: [Int] -> Int -> Int  -- part two
calculateCostOfPosition' [] _ = 0
calculateCostOfPosition' crabs@(c:cs) position =
    let
        absoluteDistance = abs (c - position)
        cost = sum [0..absoluteDistance]
    in  cost + calculateCostOfPosition' cs position


findCheapestPosition :: [Int] -> [Int] -> Int
findCheapestPosition [] _ = error "No crabs provided."
findCheapestPosition _ [] = error "No positions to assess."
findCheapestPosition _ [p] = p
findCheapestPosition crabs positions@(p0:p1:ps)
    | c0 < c1 = findCheapestPosition crabs (p0:ps)  -- discard p0 as p1 is cheaper
    | c0 >= c1 = findCheapestPosition crabs (p1:ps)  -- discard p1 as p0 is at least as cheap
    where
        c0 = calculateCostOfPosition crabs p0
        c1 = calculateCostOfPosition crabs p1


findCheapestPosition' :: [Int] -> [Int] -> Int  -- part two
findCheapestPosition' [] _ = error "No crabs provided."
findCheapestPosition' _ [] = error "No positions to assess."
findCheapestPosition' _ [p] = p
findCheapestPosition' crabs positions@(p0:p1:ps)
    | c0 < c1 = findCheapestPosition' crabs (p0:ps)  -- discard p0 as p1 is cheaper
    | c0 >= c1 = findCheapestPosition' crabs (p1:ps)  -- discard p1 as p0 is at least as cheap
    where
        c0 = calculateCostOfPosition' crabs p0
        c1 = calculateCostOfPosition' crabs p1


getPositionsRange :: [Int] -> [Int]
getPositionsRange crabs = 
    let
        s = head $ sort crabs
        l = last $ sort crabs
    in  [s..l]


findAnswer :: [Int] -> Int  -- part two
findAnswer crabs =
    let 
        positions = getPositionsRange crabs
        cheapestPosition = findCheapestPosition crabs positions
    in  calculateCostOfPosition crabs cheapestPosition


findAnswer' :: [Int] -> Int  -- part two
findAnswer' crabs =
    let 
        positions = getPositionsRange crabs
        cheapestPosition = findCheapestPosition' crabs positions
    in  calculateCostOfPosition' crabs cheapestPosition


main = do
    contents <- readFile "data.txt"
    let crabs :: [Int] = prepareData contents
    let testData :: [Int] = prepareData "16,1,2,0,4,2,7,1,2,14"
    print $ findAnswer' crabs
