import Data.Char
import Data.List
import Data.List.Split

type EdgeId = Integer

edgeLength = 10

edgeToBinString :: EdgeId -> String
edgeToBinString 0 = ""
edgeToBinString n = edgeToBinString q ++ show r
  where
    (q, r) = n `divMod` 2

binStringToEdge :: String -> EdgeId
binStringToEdge = foldl (\b a -> 2 * b + (if a == '#' || a == '1' then 1 else 0)) 0

invertEdgeId :: EdgeId -> EdgeId
invertEdgeId = binStringToEdge . reverse . padLeft . edgeToBinString
  where
    padLeft s = replicate (edgeLength - length s) '0' ++ s

getEdges :: [String] -> [EdgeId]
getEdges xs = map binStringToEdge [top, bottom, left, right]
  where
    top = head xs
    bottom = last xs
    ys = transpose xs
    left = head ys
    right = last ys

data Tile = Tile Integer [EdgeId]
  deriving (Show)

instance Eq Tile where
  (==) (Tile n _) (Tile m _) = n == m
  (/=) (Tile n _) (Tile m _) = n /= m

tileP :: String -> Tile
tileP xs = Tile id (getEdges ys)
  where
    (y : ys) = lines xs
    id = read $ takeWhile isDigit $ dropWhile (not . isDigit) y

parse :: String -> [Tile]
parse = map tileP . splitOn "\n\n"

countMatchingEdges :: EdgeId -> [Tile] -> Int
countMatchingEdges e = length . filter matchEdge
  where
    matchEdge (Tile _ es) = e `elem` (es ++ map invertEdgeId es)

isCorner :: Tile -> [Tile] -> Bool
isCorner (Tile _ es) ts = (== 2) $ length $ filter (== 1) $ map (`countMatchingEdges` ts) es

findCorners :: [Tile] -> [Tile]
findCorners ts = filter (`isCorner` ts) ts

getAns :: [Tile] -> Integer
getAns = product . map (\(Tile n _) -> n)

part1 :: [Tile] -> Integer
part1 = getAns . findCorners

example = [Tile 2311 [210, 231, 498, 89], Tile 1951 [710, 564, 841, 498], Tile 1171 [966, 24, 902, 288], Tile 1427 [948, 210, 576, 234], Tile 1489 [848, 948, 565, 18], Tile 2473 [542, 234, 966, 116], Tile 2971 [161, 85, 456, 565], Tile 2729 [85, 710, 271, 576], Tile 3079 [702, 184, 616, 264]]

main = interact $ show . part1 . parse