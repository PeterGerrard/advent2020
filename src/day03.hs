data Space = Tree | Empty
  deriving (Show, Eq)

readChar '.' = Just Empty
readChar '#' = Just Tree
readChar _ = Nothing

instance Read Space where
  readsPrec _ xs = case xs of
    [] -> []
    (x : xs) -> maybe [] (\y -> [(y, xs)]) $ readChar x
  readList xs = maybe [] (\y -> [(y, "")]) $ mapM readChar xs

parse :: String -> [[Space]]
parse = map read . lines

tailN :: Int -> [a] -> [a]
tailN 0 xs = xs
tailN _ [] = []
tailN n (_ : xs) = tailN (n -1) xs

getLocations :: Int -> Int -> [[Space]] -> [Space]
getLocations _ _ [] = []
getLocations r d xs = head (head xs) : getLocations r d (tailN d (map (tailN r) xs))

routes :: [(Int, Int)]
routes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

solvers = map (((length . filter (== Tree)) .) . uncurry getLocations) routes

solve :: [[Space]] -> [Int]
solve xs = map (\f -> f xs) solvers

main = interact $ show . product . solve . map (concat . repeat) . parse