import System.IO
import Control.Monad
import Data.Map (Map, fromList, findWithDefault)
import Data.List (nub, intersect, sort, (\\))
import Data.Maybe (isNothing)
import Debug.Trace


type ConnectionMap = Map Char Int


digitToSegments :: Map Int [Int]
digitToSegments = fromList [
    (0, [0,1,2,4,5,6]),
    (1, [2,5]),
    (2, [0,2,3,4,6]),
    (3, [0,2,3,5,6]),
    (4, [1,2,3,5]),
    (5, [0,1,3,5,6]),
    (6, [0,1,3,4,5,6]),
    (7, [0,2,5]),
    (8, [0,1,2,3,4,5,6]),
    (9, [0,1,2,3,5,6])
    ]


segmentCountToDigits :: Map Int [Int]
segmentCountToDigits = fromList [
    (2, [1]),
    (3, [7]),
    (4, [4]),
    (5, [2,3,5]),
    (6, [0,6,9]),
    (7, [8])
    ]


getOutputs :: [[String]] -> [[String]]
getOutputs = map (\d -> drop (length d - 4) d)


countOutputDigits :: [String] -> Int  -- part one
countOutputDigits [] = 0
countOutputDigits outputs@(o:os)
    | length o `elem` [2,3,4,7] = 1 + countOutputDigits os
    | otherwise = countOutputDigits os


getCandidateSegmentsFromLength :: Int -> [Int]
getCandidateSegmentsFromLength l =
    let
        possibleDigits = findWithDefault [] l segmentCountToDigits
    in  nub $ concat $ map (\d -> findWithDefault [] d digitToSegments) possibleDigits


findCandidateSegments :: Char -> [String] -> [Int]
findCandidateSegments _ [] = [0..6]
findCandidateSegments c patterns@(p:ps)
    | c `elem` p = sort $ candidateSegments `intersect` findCandidateSegments c ps
    | otherwise = findCandidateSegments c ps
    where 
        candidateSegments = getCandidateSegmentsFromLength (length p)


findMapping :: [Char] -> [Int] -> [String] -> Int -> Maybe [(Char, Int)]
findMapping [] _ _ _ = Just []
findMapping _ [] _ _ = Nothing
findMapping _ _ [] _ = Nothing
findMapping chars@(c:cs) availableSegments patterns idx
    | null candidateSegments = Nothing
    | isNothing nextMapping = findMapping chars availableSegments patterns (idx + 1)
    | otherwise = fmap ((:) (c, segment)) (findMapping cs (availableSegments \\ [segment]) patterns idx)
    where
        candidateSegments = (findCandidateSegments c patterns) `intersect` availableSegments
        segment = candidateSegments !! idx
        nextMapping = findMapping cs (availableSegments \\ [segment]) patterns idx
        

main = do
    contents <- readFile "test.txt"
    let inputs :: [[String]] = map words $ lines contents
    let patterns :: [[String]] = map (\c -> take (length c - 5) c) inputs
    let outputs :: [[String]] = getOutputs inputs
    -- print $ countOutputDigits $ concat outputs
    print $ map (\c -> findCandidateSegments c (patterns !! 1)) "abcdefg"
    print $ findMapping "abcdefg" [0..6] (patterns !! 1) 0
