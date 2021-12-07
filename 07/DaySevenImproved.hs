import System.IO
import Control.Monad


prepareData :: String -> [Int]
prepareData = (map read) . words . (map repl)
    where repl ',' = ' '
          repl c = c


type CostFunction = (Int -> Int -> Int)


linearCostFunction :: CostFunction
linearCostFunction crab position = abs (crab - position)


nonLinearCostFunction :: CostFunction  -- part two cost function
nonLinearCostFunction crab position = sum [0..(abs (crab - position))]


calculateCostOfPosition :: CostFunction -> [Int] -> Int -> Int
calculateCostOfPosition f crabs position = foldl (+) 0 $ map (\c -> f c position) crabs


findCheapestPosition :: CostFunction -> [Int] -> [Int] -> Int
findCheapestPosition _ _ [p] = p
findCheapestPosition f crabs positions@(p0:p1:ps)
    | c0 < c1 = findCheapestPosition f crabs (p0:ps)  -- discard p0 as p1 is cheaper
    | c0 >= c1 = findCheapestPosition f crabs (p1:ps)  -- discard p1 as p0 is at least as cheap
    where
        c0 = calculateCostOfPosition f crabs p0
        c1 = calculateCostOfPosition f crabs p1


findAnswer :: CostFunction -> [Int] -> Int
findAnswer f crabs =
    let 
        cheapestPosition = findCheapestPosition f crabs [(minimum crabs)..(maximum crabs)]
    in  calculateCostOfPosition f crabs cheapestPosition


main = do
    contents <- readFile "data.txt"
    let crabs :: [Int] = prepareData contents
    let testData :: [Int] = prepareData "16,1,2,0,4,2,7,1,2,14"
    print $ findAnswer linearCostFunction crabs -- part one
    print $ findAnswer nonLinearCostFunction crabs -- part two
