import Data.List

getRow :: String -> Integer
getRow = foldl (\a b -> (2 * a) + (if b == 'B' then 1 else 0)) 0

getColumn :: String -> Integer
getColumn = foldl (\a b -> (2 * a) + (if b == 'R' then 1 else 0)) 0

getId :: Integer -> Integer -> Integer
getId row column = 8 * row + column

solve :: String -> Integer
solve xs = getId row column
  where
    (rowS, colS) = splitAt 7 xs
    row = getRow rowS
    column = getColumn colS

getPossibleSeats :: [Integer]
getPossibleSeats = map (uncurry getId) [(row, column) | row <- [1 .. 126], column <- [0 .. 7]]

getMySeat :: [Integer] -> [Integer]
getMySeat xs = filter isPossible getPossibleSeats
    where isPossible x =  (x `notElem` xs) && (x-1 `elem` xs) && (x+1 `elem` xs)

part1 = interact $ show . maximum . map solve . lines

main = interact $ show . getMySeat . map solve . lines