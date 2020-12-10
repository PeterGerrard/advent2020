import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

type Adapter = Integer

part1 :: [Adapter] -> (Integer, Integer, Integer)
part1 = snd . foldl (\(p, (a, b, c)) x -> (x, (if x -1 == p then a + 1 else a, if x -2 == p then b + 1 else b, if x -3 == p then c + 1 else c))) (0, (0, 0, 0)) . sort

addBuiltIn :: [Adapter] -> [Adapter]
addBuiltIn xs = (maximum xs + 3) : xs

toAnswer1 :: (Integer, Integer, Integer) -> Integer
toAnswer1 (a, _, c) = a * c

part2 :: Map Adapter Integer -> [Adapter] -> Map Adapter Integer
part2 m [] = m
part2 m (x : xs) = part2 (Map.insert x v m) xs
  where
    v = (if x <= 3 then 1 else 0) + v1
    v1 = sum $ map (fromMaybe 0 . (`Map.lookup` m)) [x - 1, x - 2, x - 3]

main = interact $ show . snd . Map.findMax . part2 Map.empty . sort . addBuiltIn . map read . lines