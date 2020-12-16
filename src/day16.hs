import Data.Ix (inRange)
import Data.List
import Data.List.Split

type Range = (Int, Int)

data Rule = Rule Int String [Range]
  deriving (Eq, Show)

rId :: Rule -> Int
rId (Rule id _ _) = id

rName :: Rule -> String
rName (Rule _ name _) = name

checkRule :: Rule -> Int -> Bool
checkRule (Rule _ _ rs) i = any (`inRange` i) rs

rangeP :: String -> Range
rangeP s = (x, y)
  where
    [x, y] = map read $ splitOn "-" s

ruleP :: Int -> String -> Rule
ruleP id s = Rule id n rs
  where
    [n, xs] = splitOn ": " s
    rs = map rangeP $ splitOn " or " xs

type Ticket = [Int]

ticketP :: String -> Ticket
ticketP = map read . splitOn ","

invalidValues :: [Rule] -> Ticket -> [Int]
invalidValues rs = filter (\i -> not $ any (`checkRule` i) rs)

discardInvalidTickets :: [Rule] -> [Ticket] -> [Ticket]
discardInvalidTickets rs = filter (null . invalidValues rs)

matchingRules :: [Int] -> [Rule] -> [Rule]
matchingRules xs = filter (\r -> all (checkRule r) xs)

collapse :: (Eq a) => [[a]] -> [a]
collapse = go []
  where
    go vs xs
      | all ((== 1) . length) xs = concat xs
      | otherwise = go (vs ++ newvs) $ map (\zs -> if length zs == 1 then zs else zs \\ newvs) xs
      where
        newvs = concat (filter ((== 1) . length) xs) \\ vs

getRuleOrder :: [Rule] -> [Ticket] -> [Rule]
getRuleOrder rs ts = collapse potentialRules
  where
    xs = transpose ts
    potentialRules = map (`matchingRules` rs) xs

data Input = Input [Rule] Ticket [Ticket]
  deriving (Show)

inputP :: String -> Input
inputP s = Input rs t ts
  where
    [a, b, c] = splitOn "\n\n" s
    rs = zipWith ruleP [0 ..] $ lines a
    t = ticketP $ lines b !! 1
    ts = map ticketP $ drop 1 $ lines c

part1 :: Input -> Int
part1 (Input rs _ ts) = sum $ concatMap (invalidValues rs) ts

-- part2 :: Input -> Int
part2 (Input rs t ts) = getValue $ map fst $ getDepRules $ zip [0 ..] $ getRuleOrder rs (discardInvalidTickets rs ts)
  where
    getDepRules = filter (\(_, r) -> isPrefixOf "departure" $ rName r)
    getValue = product . map (t !!)

main = interact $ show . part2 . inputP