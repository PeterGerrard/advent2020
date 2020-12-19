import Control.Arrow
import Data.List
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map

type RuleId = Integer

data Rule = MatchChar Char | MatchRule RuleId | Cons [Rule]
  deriving (Show)

ruleP :: String -> Rule
ruleP s = if length rs == 1 then f (head rs) else Cons (map f rs)
  where
    rs = splitOn " " s
    f ['"', x, '"'] = MatchChar x
    f xs = MatchRule (read xs)

rulesP :: String -> (RuleId, [Rule])
rulesP s = (read i, map ruleP rs)
  where
    [i, r] = splitOn ": " s
    rs = splitOn " | " r

type RuleSet = Map RuleId [Rule]

ruleSetP :: String -> Map RuleId [Rule]
ruleSetP = Map.fromList . map rulesP . lines

reduceResults :: Eq a => [Maybe [a]] -> Maybe [a]
reduceResults = f . nub . concat . collapse
  where
    f [] = Nothing
    f xs = Just xs
    collapse [] = []
    collapse (Nothing : xs) = collapse xs
    collapse (Just x : xs) = x : collapse xs

matches :: RuleSet -> Rule -> String -> Maybe [String]
matches _ (Cons []) [] = Just [""]
matches _ _ [] = Nothing
matches _ (MatchChar c) (x : xs) = if x == c then Just [xs] else Nothing
matches ruleSet (MatchRule rId) xs = Map.lookup rId ruleSet >>= flip (match ruleSet) xs
matches _ (Cons []) xs = Just [xs]
matches ruleSet (Cons (r : rs)) xs = matches ruleSet r xs >>= reduceResults . map (matches ruleSet (Cons rs))

match :: RuleSet -> [Rule] -> String -> Maybe [String]
match ruleSet rs s = reduceResults $ map (\r -> matches ruleSet r s) rs

match0 :: RuleSet -> String -> Bool
match0 rs s = maybe False (any null) $ Map.lookup 0 rs >>= \r -> match rs r s

parse :: String -> (RuleSet, [String])
parse s = (ruleSetP x, lines xs)
  where
    [x, xs] = splitOn "\n\n" s

solve :: RuleSet -> [String] -> Int
solve rs = length . filter (match0 rs)

part2Changes :: RuleSet -> RuleSet
part2Changes = Map.insert 8 [MatchRule 42, Cons [MatchRule 42, MatchRule 8]] . Map.insert 11 [Cons [MatchRule 42, MatchRule 31], Cons [MatchRule 42, MatchRule 11, MatchRule 31]]

main = interact $ show . uncurry solve . first part2Changes . parse