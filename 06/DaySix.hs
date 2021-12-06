import System.IO
import Control.Monad
import Data.List (sort, group, sortBy)
import Data.Ord (comparing)


reproductionTime :: Int = 6
newFishReproductionTime :: Int = 8


data Timer = Timer {daysLeft :: Int, count :: Int} deriving (Show, Ord, Eq)


prepareData :: String -> [Timer]
prepareData dataString =
    let 
        characters = words $ map (\c -> if c == ',' then ' ' else c) dataString
        createTimers l = Timer (head l) (length l)
    in  map createTimers $ group $ sort $ map (read) characters


simulateDay :: [Timer] -> [Timer]
simulateDay [] = []
simulateDay timers@(t:ts)
    | d == 0 = [Timer reproductionTime c] ++ [Timer newFishReproductionTime c] ++ simulateDay ts
    | otherwise = [Timer (d - 1) c] ++ simulateDay ts
    where
        d = daysLeft t
        c = count t


sortTimers :: [Timer] -> [Timer]
sortTimers timers = sortBy (comparing daysLeft) timers


groupTimers :: [Timer] -> [Timer]
groupTimers [] = []
groupTimers (t0:t1:ts)
    | d0 == d1 = [Timer d0 (c0 + c1)] ++ ts  -- group same day counts together
    | otherwise = [t0] ++ [t1] ++ ts
    where
        d0 = daysLeft t0
        d1 = daysLeft t1
        c0 = count t0
        c1 = count t1


runSimulation :: [Timer] -> Int -> [Timer]
runSimulation [] _ = []
runSimulation timers 0 = timers
runSimulation timers days
    | days == 0 = newTimers
    | otherwise = runSimulation newTimers (days - 1)
    where newTimers = groupTimers$ sortTimers $ simulateDay timers


countFish :: [Timer] -> Int
countFish timers = sum $ map (count) timers


main = do
    contents <- readFile "data.txt"
    let initialPop :: [Timer] = prepareData contents
    let testData :: [Timer] = prepareData "3,4,3,1,2"
    print $ countFish $ runSimulation initialPop 256
