import System.IO
import Control.Monad
import Data.Char (digitToInt)
import Data.List (sort, group, span)


data Point = Point {x :: Int, y :: Int} deriving (Eq, Ord, Show)


replaceComma :: Char -> Char
replaceComma ',' = ' '
replaceComma c = c


getEndPointsFromDefinition :: String -> [Point]
getEndPointsFromDefinition def = 
    let
        (p1:_:p2:[]) = words def
        (x1:y1:[]) = words $ map replaceComma p1
        (x2:y2:[]) = words $ map replaceComma p2
        point1 = Point (read x1 :: Int) (read y1 :: Int)
        point2 = Point (read x2 :: Int) (read y2 :: Int)
    in  [point1] ++ [point2]


getPointsFromEndPoints :: [Point] -> [Point]
getPointsFromEndPoints [start, end]
    | x1 == x2 && y1 == y2 = []  -- start and end points are the same
    | x1 < x2 && y1 == y2 = [Point (x1 + 1) y1] ++ getPointsFromEndPoints [(Point (x1 + 1) y1), end]
    | x1 == x2 && y1 < y2 = [Point x1 (y1 + 1)] ++ getPointsFromEndPoints [(Point x1 (y1 + 1)), end]
    | x1 > x2 && y1 == y2 = [Point (x1 - 1) y1] ++ getPointsFromEndPoints [(Point (x1 - 1) y1), end]
    | x1 == x2 && y1 > y2 = [Point x1 (y1 - 1)] ++ getPointsFromEndPoints [(Point x1 (y1 - 1)), end]
    | x1 < x2 && y1 < y2 = [Point (x1 + 1) (y1 + 1)] ++ getPointsFromEndPoints [(Point (x1 + 1) (y1 + 1)), end]
    | x1 > x2 && y1 < y2 = [Point (x1 - 1) (y1 + 1)] ++ getPointsFromEndPoints [(Point (x1 - 1) (y1 + 1)), end]
    | x1 < x2 && y1 > y2 = [Point (x1 + 1) (y1 - 1)] ++ getPointsFromEndPoints [(Point (x1 + 1) (y1 - 1)), end]
    | x1 > x2 && y1 > y2 = [Point (x1 - 1) (y1 - 1)] ++ getPointsFromEndPoints [(Point (x1 - 1) (y1 - 1)), end]
    where
        x1 = x start
        y1 = y start
        x2 = x end
        y2 = y end


getAllPoints :: [[Point]] -> [[Point]]
getAllPoints [] = []
getAllPoints (p:ps)
    | null points = getAllPoints ps  -- ignoring empty lines
    | not $ start `elem` points = [[start] ++ points] ++ getAllPoints ps
    | not $ end `elem` points = [points ++ [end]] ++ getAllPoints ps
    | otherwise = [points] ++ getAllPoints ps
    where
        points = getPointsFromEndPoints p
        start = head p
        end = last p


countOverlaps :: [[Point]] -> Int
countOverlaps [] = 0
countOverlaps points = 
    let
        sorted = sort $ concat points  -- flattens into a single list
        hasOverlap p = if (length p) >= 2 then 1 else 0
    in  sum $ map hasOverlap (group $ sorted)


main = do
    contents <- readFile "data.txt"
    let lineDefinitions :: [String] = lines $ contents
    let endPoints :: [[Point]] = map getEndPointsFromDefinition lineDefinitions
    let allPoints :: [[Point]] = getAllPoints endPoints
    print $ countOverlaps allPoints
