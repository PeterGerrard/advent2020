data Maths = Number Integer | Add | Multiply | SubProblem [Maths]
  deriving (Show)

mathsP :: String -> [Maths]
mathsP s = if null ss then ms else error "Failed parse"
  where
    (ms, ss) = go [] s
    go acc [] = (acc, [])
    go acc (' ' : xs) = go acc xs
    go acc (')' : xs) = (acc, xs)
    go acc ('(' : xs) = go (acc ++ [SubProblem sub]) ys
      where
        (sub, ys) = go [] xs
    go acc ('+' : xs) = go (acc ++ [Add]) xs
    go acc ('*' : xs) = go (acc ++ [Multiply]) xs
    go acc xs = go (acc ++ [Number n]) ys
      where
        [(n, ys)] = reads xs

solve :: [Maths] -> Integer
solve = go Nothing Nothing
  where
    go :: Maybe Integer -> Maybe (Integer -> Integer) -> [Maths] -> Integer
    go (Just n) Nothing [] = n
    go Nothing Nothing (Number n : xs) = go (Just n) Nothing xs
    go x y (SubProblem ms : xs) = go x y (Number (solve ms) : xs)
    go Nothing (Just f) (Number n : xs) = go (Just $ f n) Nothing xs
    go (Just x) Nothing (Add : xs) = go Nothing (Just $ (+) x) xs
    go (Just x) Nothing (Multiply : xs) = go Nothing (Just $ (*) x) xs

part1 :: String -> Integer
part1 = sum . map (solve . mathsP) . lines

solveSubProblems :: [Maths] -> [Maths]
solveSubProblems = map f
  where
    f (SubProblem ms) = Number (solve2 ms)
    f x = x

solveAdd :: [Maths] -> [Maths]
solveAdd (Number x : Add : Number y : xs) = solveAdd (Number (x + y) : xs)
solveAdd [] = []
solveAdd (x : xs) = x : solveAdd xs

solveMultiply :: [Maths] -> Integer
solveMultiply (Number x : xs) = go x xs
  where
    go acc [] = acc
    go acc (Number n : xs) = go (acc * n) xs
    go acc (Multiply : xs) = go acc xs

solve2 :: [Maths] -> Integer
solve2 = solveMultiply . solveAdd . solveSubProblems

part2 :: String -> Integer
part2 = sum . map (solve2 . mathsP) . lines

main = interact $ show . part2