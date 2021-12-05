import System.IO
import Control.Monad
import Data.Array
import Data.Char (digitToInt)


gridSize :: Int = 5


getNumbers :: String -> [String]
getNumbers numbersString =
    let
        repl ',' = ' '
        repl c = c
    in  words $ map repl numbersString


getCards :: [String] -> [[String]]
getCards [] = []
getCards cardsString =
    let
        (card, rest) = splitAt (gridSize * gridSize) cardsString
    in  [card] ++ getCards rest


markCard :: String -> [String] -> [String]
markCard _ [] = []
markCard number (first:rest) = [(if first == number then "X" else first)] ++ markCard number rest


markCards :: String -> [[String]] -> [[String]]
markCards number cards = map (markCard number) cards


checkRow :: [String] -> Int -> Bool
checkRow card idx =
    let 
        from = idx * gridSize
        to = from + gridSize - 1
        row = take (to - from + 1) (drop from card)
    in  all (=="X") row


checkCol :: [String] -> Int -> Bool
checkCol card idx =
    let 
        columnIndices = [i | i <- [0..(gridSize * gridSize - 1)], i `mod` gridSize == idx]
        col = map (card !!) columnIndices
    in  all (=="X") col


hasWon :: [String] -> Bool
hasWon card = 
    let
        rowCheck = any (== True) $ map (checkRow card) [0..(gridSize - 1)]
        colCheck = any (== True) $ map (checkCol card) [0..(gridSize - 1)]
    in  rowCheck || colCheck


playRound :: [[String]] -> String -> [[String]]
playRound cards number
    | winningCards /= [] = [head winningCards]
    | otherwise = markedCards
    where 
        markedCards = markCards number cards
        winningCards = [card | card <- markedCards, hasWon card]


playLosingRound :: [[String]] -> String -> [[String]]
playLosingRound cards number
    | length markedCards == 1 && hasWon (head markedCards) = markedCards
    | otherwise = losingCards
    where
        markedCards = markCards number cards
        losingCards = [card | card <- markedCards, not $ hasWon card]


scoreCard :: [String] -> String -> Int 
scoreCard card number =
    let
        unmarkedNumbers = map (\x -> read x :: Int) [n | n <- card, n /= "X"]
    in  (read number :: Int) * sum unmarkedNumbers


scoreWinningCard :: [[String]] -> [String] -> Int
scoreWinningCard [] _ = 0
scoreWinningCard _ [] = 0
scoreWinningCard cards (number : rest)
    | length markedCards == 1 = scoreCard (head markedCards) number
    | otherwise = scoreWinningCard markedCards rest
    where
        markedCards = playRound cards number


scoreLosingCard :: [[String]] -> [String] -> Int
scoreLosingCard [] _ = 0
scoreLosingCard _ [] = 0
scoreLosingCard cards (number : rest)
    | length markedCards == 1 && hasWon (head markedCards) = scoreCard (head markedCards) number
    | otherwise = scoreLosingCard markedCards rest
    where markedCards = playLosingRound cards number


main = do
    numberContents <- readFile "numbers.txt"
    cardsContents <- readFile "cards.txt"
    let numbers :: [String] = getNumbers numberContents
    let cards :: [[String]] = getCards $ words cardsContents

    print $ scoreWinningCard cards numbers
    print $ scoreLosingCard cards numbers
