getRow :: String -> Integer
getRow = foldl (\a b -> (2 * a) + (if b == 'B' then 1 else 0)) 0

getColumn :: String -> Integer
getColumn = foldl (\a b -> (2 * a) + (if b == 'R' then 1 else 0)) 0

getSeatId :: String -> Integer
getSeatId xs = 8 * row + column
  where
    (rowS, colS) = splitAt 7 xs
    row = getRow rowS
    column = getColumn colS

getMySeat :: [Integer] -> [Integer]
getMySeat xs = filter (`notElem` xs) [x + 1 | x <- xs, y <- xs, x + 2 == y]

part1 = interact $ show . maximum . map getSeatId . lines

main = interact $ show . getMySeat . map getSeatId . lines