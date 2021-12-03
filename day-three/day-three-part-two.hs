import System.IO
import Control.Monad
import Numeric (readInt)
import Data.Char (digitToInt)


getByteFromIndex :: Int -> String -> Char
getByteFromIndex idx s = s !! idx


getMostCommonBit :: [String] -> Int -> Char
getMostCommonBit binaries idx =
    let 
        bytes = map (getByteFromIndex idx) binaries
        ones = [i | i <- bytes, i == '1']
        zeros = [i | i <- bytes, i == '0']
    in  if length ones >= length zeros then '1' else '0'


getLeastCommonBit :: [String] -> Int -> Char
getLeastCommonBit binaries idx =
    let 
        bytes = map (getByteFromIndex idx) binaries
        ones = [i | i <- bytes, i == '1']
        zeros = [i | i <- bytes, i == '0']
    in  if length ones >= length zeros then '0' else '1'


findOxygenGeneratorRating :: [String] -> Int -> Int
findOxygenGeneratorRating [binary] _ = getIntFromBinary binary
findOxygenGeneratorRating binaries idx =
    let
        mostCommonBit = getMostCommonBit binaries idx
        remainingBinaries = [binary | binary <- binaries, (binary !! idx) == mostCommonBit]
    in findOxygenGeneratorRating remainingBinaries (idx + 1)


findCO2ScrubberRating :: [String] -> Int -> Int
findCO2ScrubberRating [binary] _ = getIntFromBinary binary
findCO2ScrubberRating binaries idx =
    let
        mostCommonBit = getMostCommonBit binaries idx
        remainingBinaries = [binary | binary <- binaries, (binary !! idx) /= mostCommonBit]
    in  findCO2ScrubberRating remainingBinaries (idx + 1)

calculateLifeSupportRating :: [String] -> Int
calculateLifeSupportRating binaries =
    let
        oxygenGeneratorRating = findOxygenGeneratorRating binaries 0
        co2ScrubberRating = findCO2ScrubberRating binaries 0
    in  oxygenGeneratorRating * co2ScrubberRating


getIntFromBinary :: [Char] -> Int
getIntFromBinary binary = 
    let 
        binaryLength = length binary
        getValue idx byte = 2^(binaryLength - 1 - idx) * (digitToInt byte)
    in  sum [getValue idx (binary !! idx) | idx <- [0..binaryLength-1]]


main = do  
        contents <- readFile "data.txt"
        let list :: [String] = words $ contents
        print $ findOxygenGeneratorRating list 0
        print $ findCO2ScrubberRating list 0
        print $ calculateLifeSupportRating list
