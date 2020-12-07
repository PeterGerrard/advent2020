import Data.List
import Data.List.Split
import qualified Data.Map as M
import Data.Maybe

type BagColour = String

type BagColourMap = M.Map BagColour [(BagColour, Int)]

toBagColour :: String -> BagColour
toBagColour = unwords . take 2 . words

parseContainsSection' :: String -> (BagColour, Int)
parseContainsSection' s = (toBagColour b, i)
  where
    [(i, b)] = (reads :: ReadS Int) s

parseContainsSection :: String -> [(BagColour, Int)]
parseContainsSection " no other bags." = []
parseContainsSection s = map parseContainsSection' ss
  where
    ss = splitOn "," s

parseLine :: String -> (BagColour, [(BagColour, Int)])
parseLine line = (toBagColour c, isT)
  where
    (c : is) = splitOn "contain" line
    isT = concatMap parseContainsSection is

parse :: String -> BagColourMap
parse = M.fromList . map parseLine . lines

bagContains :: BagColour -> BagColour -> BagColourMap -> Bool
bagContains lookup currentColour m =
  lookup == currentColour
    || any (\c -> bagContains lookup c m) (maybe [] (map fst) (M.lookup currentColour m))

part1 :: BagColourMap -> Int
part1 m = length $ filter (\x -> bagContains myBag x m) keys
  where
    keys = M.keys m \\ [myBag]
    myBag = "shiny gold"

totalBags :: BagColourMap -> BagColour -> Int
totalBags m c = 1 + sum (map (\(x, y) -> totalBags m x * y) bs)
  where
    bs = fromMaybe [] $ M.lookup c m

part2 :: BagColourMap -> Int
part2 m = totalBags m "shiny gold" - 1

main = interact $ show . part2 . parse