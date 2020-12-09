import Data.List.Extra

findFirstNonSum :: (Num a, Eq a) => [a] -> [a] -> Maybe a
findFirstNonSum _ [] = Nothing
findFirstNonSum xs (y : ys) = if null [() | a <- xs, b <- xs, a + b == y] then Just y else findFirstNonSum (snoc (tail xs) y) ys

solve :: (Num a, Eq a) => Int -> [a] -> Maybe a
solve n xs = findFirstNonSum (take n xs) (drop n xs)

part1 = solve 25

findSummers :: (Num a, Eq a, Ord a) => [a] -> a -> [a]
findSummers xs t = if sum potential == t then potential else findSummers (tail xs) t
  where
    potential = head $ dropWhile (\ys -> sum ys < t) (inits xs)

part2 xs = (\x -> minimum x + maximum x) . findSummers xs <$> part1 xs

main = interact $ show . part2 . map read . lines