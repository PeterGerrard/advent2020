import Data.List
import Data.List.Split

part1 = interact $ show . sum . map (length . nub . concat . lines) . splitOn "\n\n"

solve :: [String] -> Int
solve [] = 0
solve (x : xs) = length $ filter (\c -> all (c `elem`) xs) x

main = interact $ show . sum . map (solve . lines) . splitOn "\n\n"