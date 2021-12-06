import System.IO
import Control.Monad
import Data.List (sort, group, sortBy)
import Data.Ord (comparing)


reproductionTime :: Int = 6
newFishReproductionTime :: Int = 8


data School = School {daysLeft :: Int, count :: Int} deriving (Show, Eq, Ord)


prepareData :: String -> [School]
prepareData dataString =
    let 
        characters = words $ map (\c -> if c == ',' then ' ' else c) dataString
        createTimers l = School (head l) (length l)
    in  map createTimers $ group $ sort $ map (read) characters


simulateDay :: [School] -> [School]
simulateDay [] = []
simulateDay schools@(s:rest)
    | d == 0 = [School reproductionTime c] ++ [School newFishReproductionTime c] ++ simulateDay rest
    | otherwise = [School (d - 1) c] ++ simulateDay rest
    where
        d = daysLeft s
        c = count s


sortSchools :: [School] -> [School]
sortSchools schools = sortBy (comparing daysLeft) schools


groupSchools :: [School] -> [School]
groupSchools [] = []
groupSchools (s0:s1:rest)
    | d0 == d1 = [School d0 (c0 + c1)] ++ rest  -- group same day counts together
    | otherwise = [s0] ++ [s1] ++ rest
    where
        d0 = daysLeft s0
        d1 = daysLeft s1
        c0 = count s0
        c1 = count s1


runSimulation :: [School] -> Int -> [School]
runSimulation [] _ = []
runSimulation schools 0 = schools
runSimulation schools days
    | days == 0 = newSchools
    | otherwise = runSimulation newSchools (days - 1)
    where newSchools = groupSchools $ sortSchools $ simulateDay schools


countFish :: [School] -> Int
countFish schools = sum $ map (count) schools


main = do
    contents <- readFile "data.txt"
    let initialPop :: [School] = prepareData contents
    let testData :: [School] = prepareData "3,4,3,1,2"
    print $ countFish $ runSimulation initialPop 256
