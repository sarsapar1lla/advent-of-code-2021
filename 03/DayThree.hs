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
    in  if length ones > length zeros then '1' else '0'


getLeastCommonBit :: [String] -> Int -> Char
getLeastCommonBit binaries idx =
    let 
        bytes = map (getByteFromIndex idx) binaries
        ones = [i | i <- bytes, i == '1']
        zeros = [i | i <- bytes, i == '0']
    in  if length ones > length zeros then '0' else '1'


getMostCommonBits :: [String] -> [Char]
getMostCommonBits binaries = [getMostCommonBit binaries idx | idx <- [0..binaryLength-1]]
    where binaryLength = length (binaries !! 0)


getLeastCommonBits :: [String] -> [Char]
getLeastCommonBits binaries = [getLeastCommonBit binaries idx | idx <- [0..binaryLength-1]]
    where binaryLength = length (binaries !! 0)


getIntFromBinary :: [Char] -> Int
getIntFromBinary binary = 
    let 
        binaryLength = length binary
        getValue idx byte = 2^(binaryLength - 1 - idx) * (digitToInt byte)
    in  sum [getValue idx (binary !! idx) | idx <- [0..binaryLength-1]]


calculatePowerConsumption :: [String] -> Int
calculatePowerConsumption binaries = 
    let 
        gammaRate = getIntFromBinary $ getMostCommonBits binaries
        epsilonRate = getIntFromBinary $ getLeastCommonBits binaries
    in  gammaRate * epsilonRate


main = do  
        contents <- readFile "data.txt"
        let list :: [String] = words $ contents
        print $ calculatePowerConsumption list
