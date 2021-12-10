import System.IO
import Control.Monad
import Data.Map (Map, fromList, (!), keys, elems)


data Braket = Braket {index :: Int, char :: Char} deriving (Show)


brakets :: Map Char Char = fromList [
    ('(', ')'),
    ('[', ']'),
    ('{', '}'),
    ('<', '>')
    ]


findOpeningBrakets :: [Char] -> Int -> [Braket]
findOpeningBrakets [] _ = []
findOpeningBrakets (c:cs) idx
    | c `elem` (keys brakets) = [Braket idx c] ++ findOpeningBrakets cs (idx + 1)
    | otherwise = findOpeningBrakets cs (idx + 1)


findClosingBrakets :: [Char] -> Int -> [Braket]
findClosingBrakets [] _ = []
findClosingBrakets (c:cs) idx
    | c `elem` (elems brakets) = [Braket idx c] ++ findClosingBrakets cs (idx + 1)
    | otherwise = findClosingBrakets cs (idx + 1)


findIllegalChar :: [Braket] -> [Braket] -> MaybeChar
findIllegalChar openers@(o:os) closers
    |
    where 


main = do
    contents <- readFile "test.txt"
    let allLines = lines contents
    print $ allLines !! 0
    print $ findOpeningBrakets (allLines !! 0) 0
    print $ findClosingBrakets (allLines !! 0) 0
