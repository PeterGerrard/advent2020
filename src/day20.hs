{-# LANGUAGE TupleSections #-}

import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

data Tile = Tile Integer [[Char]]
  deriving (Show)

instance Eq Tile where
  (==) (Tile n xs) (Tile m ys) = n == m && xs == ys
  (/=) (Tile n xs) (Tile m ys) = n /= m || xs /= ys

tileP :: String -> Tile
tileP xs = Tile id ys
  where
    (y : ys) = lines xs
    id = read $ takeWhile isDigit $ dropWhile (not . isDigit) y

parse :: String -> [Tile]
parse = map tileP . splitOn "\n\n"

countMatchingEdges :: Tile -> [Tile] -> Int
countMatchingEdges e@(Tile n _) es = length . filter (\c -> isJust (extendRow c es')) $ (rots e)
  where es' = filter (\(Tile m _) -> m /= n) es

isCorner :: Tile -> [Tile] -> Bool
isCorner t ts = (== 2) $ countMatchingEdges t ts

isEdge :: Tile -> [Tile] -> Bool
isEdge t ts = (== 3) $ countMatchingEdges t ts

findCorners :: [Tile] -> [Tile]
findCorners ts = filter (`isCorner` ts) ts

getAns :: [Tile] -> Integer
getAns = product . map (\(Tile n _) -> n)

findEdges :: [Tile] -> [Tile]
findEdges ts = filter (`isEdge` ts) ts

right :: Tile -> String
right (Tile _ xs) = last . transpose $ xs

left :: Tile -> String
left (Tile _ xs) = head . transpose $ xs

bottom :: Tile -> String
bottom (Tile _ xs) = last xs

top :: Tile -> String
top (Tile _ xs) = head xs

rot :: Tile -> Tile
rot (Tile n xs) = Tile n . map reverse $ transpose xs

rot' :: [(Int,Int)] -> [(Int,Int)]
rot' ps = map (\(x,y) -> (y, mx-x)) ps
  where mx = maximum $ map fst ps

flipX :: Tile -> Tile
flipX (Tile n xs) = Tile n $ map reverse xs

flipX' :: [(Int,Int)] -> [(Int,Int)]
flipX' ps = map (\(x,y) -> (mx-x,y)) ps
  where mx = maximum $ map fst ps

flipY :: Tile -> Tile
flipY (Tile n xs) = Tile n $ reverse xs

flipY' :: [(Int,Int)] -> [(Int,Int)]
flipY' ps = map (\(x,y) -> (x,my-y)) ps
  where my = maximum $ map snd ps

rots :: Tile -> [Tile]
rots = take 4 . iterate rot

rots' :: [(Int,Int)] -> [[(Int,Int)]]
rots' = take 4 . iterate rot'

orientations :: Tile -> [Tile]
orientations t = nub $ concatMap rots [t, flipX t, flipY t, flipX $ flipY t]

orientations' :: [(Int,Int)] -> [[(Int,Int)]]
orientations' t = nub $ concatMap rots' [t, flipX' t, flipY' t, flipX' $ flipY' t]

matchRight :: Tile -> Tile -> Maybe Tile
matchRight t1 t2 = if null os then Nothing else Just (head os)
  where
    e1 = right t1
    os = filter ((==e1) . left) $ orientations t2

matchBottom :: Tile -> Tile -> Maybe Tile
matchBottom t1 t2 = if null os then Nothing else Just (head os)
  where
    e1 = bottom t1
    os = filter ((==e1) . top) $ orientations t2

safeHead :: [Maybe a] -> Maybe a
safeHead [] = Nothing
safeHead ((Just x):_) = Just x
safeHead (_:xs) = safeHead xs

extendRow :: Tile -> [Tile] -> Maybe (Tile, [Tile])
extendRow x ts = fmap (\(t',t) -> (t', ts \\ [t])) . safeHead $ map (\t -> fmap (,t) (matchRight x t)) ts

extendCol :: Tile -> [Tile] -> Maybe (Tile, [Tile])
extendCol x ts = fmap (\(t',t) -> (t', ts \\ [t])) . safeHead $ map (\t -> fmap (,t) (matchBottom x t)) ts

getRow :: Tile -> [Tile] -> ([Tile], [Tile])
getRow t = go [t]
  where
    go (r:rs) ts = case extendRow r ts of
      Just (r', ts') -> go (r':r:rs) ts'
      Nothing -> (reverse (r:rs), ts)

findMap :: [Tile] -> [[Tile]]
findMap ts = go c1' ts'
  where
    c1@(Tile n _) = head $ findCorners ts
    [c1'] = filter (\c -> isJust (extendRow c ts') && isJust (extendCol c ts')) (rots c1)
    ts' = filter (\(Tile m _) -> m /= n) ts
    go :: Tile -> [Tile] -> [[Tile]]
    go c cs = rs : (if null cs' then [] else go c' cs'')
      where
        (rs, cs') = getRow c cs
        Just (c', cs'') = extendCol (head rs) cs'


part1 :: [Tile] -> Integer
part1 = getAns . cs . findMap
  where
    cs xs = map (\f -> f xs) [head.head,head.last,last.head,last.last]

trimEdges :: Tile -> [[Char]]
trimEdges (Tile _ xs) = map (init . tail) . init . tail $ xs

createImageRow :: [Tile] -> [[Char]]
createImageRow = foldl (zipWith (++)) (repeat []) . map trimEdges

createImage :: [[Tile]] -> [[Char]]
createImage = concatMap createImageRow

seaMonster :: [(Int,Int)]
seaMonster = [(18,0),(0,1),(5,1),(6,1),(11,1),(12,1),(17,1),(18,1),(19,1),(1,2),(4,2),(7,2),(10,2),(13,2),(16,2)]

toMap :: [[a]] -> Map (Int,Int) a
toMap = Map.fromList . concat . zipWith (\y -> zipWith (\x c -> ((x,y),c)) [0..]) [0..]

data Sea = Calm | Wave | Monster deriving (Eq)

instance Show Sea where
  show Calm = "."
  show Wave = "#"
  show Monster = "O"

toSeaMap :: [[Char]] -> Map (Int,Int) Sea
toSeaMap = Map.map (\c -> if c=='#' then Wave else Calm) . toMap

findMonster :: [(Int,Int)] -> Map (Int,Int) Sea -> Map (Int,Int) Sea
findMonster ms m = foldl update m . concat . filter allMatch $ map (\(ox,oy) -> map (\(x,y) -> (x+ox,y+oy)) ms) offsets
  where
    allMatch = all ((maybe False (/=Calm)) . (m Map.!?))
    offsets = [(x,y) | x <- [0..mx], y <- [0..my]]
    (mx,my) = maximum $ Map.keys m
    update m ms = Map.insert ms Monster m

findAllMonsters m = foldl (flip findMonster) m (orientations' seaMonster)

part2 = Map.size . Map.filter (==Wave) . findAllMonsters .toSeaMap . createImage . findMap

printMap m = unlines [concat [show (m Map.! (x,y)) | x <- [0..mx]] | y <- [0..my]]
  where (mx,my) = maximum $ Map.keys m

main = interact $ show . part2 . parse

ex = [".#.#..#.##...#.##..#####",
  "###....#.#....#..#......",
  "##.##.###.#.#..######...",
  "###.#####...#.#####.#..#",
  "##.#....#.##.####...#.##",
  "...########.#....#####.#",
  "....#..#...##..#.#.###..",
  ".####...#..#.....#......",
  "#..#.##..#..###.#.##....",
  "#.####..#.####.#.#.###..",
  "###.#.#...#.######.#..##",
  "#.####....##..########.#",
  "##..##.#...#...#.#.#.#..",
  "...#..#..#.#.##..###.###",
  ".#.#....#.##.#...###.##.",
  "###.#...#..#.##.######..",
  ".#.#.###.##.##.#..#.##..",
  ".####.###.#...###.#..#.#",
  "..#.#..#..#.#.#.####.###",
  "#..####...#.#.#.###.###.",
  "#####..#####...###....##",
  "#.##..#..#...#..####...#",
  ".#.###..##..##..####.##.",
  "...###...##...#...#..###"]