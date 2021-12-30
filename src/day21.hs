import Data.List
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map

type Allergen = String
type Ingredient = String

parseIngredientLine :: String -> ([Ingredient], [Allergen])
parseIngredientLine s = (words is,as')
    where
        [is,as] = splitOn "(" s
        as' = splitOn ", " (init $drop 9 as)

parse :: String -> [([Ingredient], [Allergen])]
parse = map parseIngredientLine . lines

getPotentialAllergens :: ([Ingredient], [Allergen]) -> [Map Allergen Ingredient]
getPotentialAllergens (is, as) = map Map.fromList . nub . map (zip as) $ permutations is

uniqueValues :: Map Allergen Ingredient -> Bool
uniqueValues m = nub (Map.elems m) == Map.elems m

canCoexist :: Map Allergen Ingredient -> Map Allergen Ingredient -> Bool
canCoexist m1 m2 = (all id . Map.elems $ Map.intersectionWith (==) m1 m2) && (uniqueValues $ Map.union m1 m2)

getMaps :: Map Allergen [Ingredient] -> [Map Allergen Ingredient]
getMaps = go [Map.empty] . Map.toList
    where
        go :: [Map Allergen Ingredient] -> [(Allergen, [Ingredient])] -> [Map Allergen Ingredient]
        go acc [] = acc
        go acc ((a,is):ms) = go (concatMap get acc) ms
            where
                get :: Map Allergen Ingredient -> [Map Allergen Ingredient]
                get m = map (\i -> Map.insert a i m) $ is \\ Map.elems m

findPotentials :: [([Ingredient], [Allergen])] -> Map Allergen [Ingredient]
findPotentials = foldl (Map.unionWith intersect) Map.empty . map (\(is,as) -> Map.fromList [(a,is) | a <- as])

solve = head . getMaps . findPotentials

part1 = show . length . (\(x,m) -> filter (`notElem` m) x) . (\x -> (concatMap fst x, Map.elems $ solve x)) . parse

part2 = intercalate "," .  map snd . sort . Map.toList . solve . parse

main = interact $ part2

example = "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)\ntrh fvjkl sbzzf mxmxvkd (contains dairy)\nsqjhc fvjkl (contains soy)\nsqjhc mxmxvkd sbzzf (contains fish)"