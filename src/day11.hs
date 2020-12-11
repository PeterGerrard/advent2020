import Control.Monad
import Data.IntMap (IntMap)
import qualified Data.IntMap as Map
import Data.Maybe

data Space = Floor | Empty | Taken
  deriving (Eq)

readSpace :: Char -> Space
readSpace '.' = Floor
readSpace 'L' = Empty
readSpace '#' = Taken

instance Show Space where
  show Floor = "."
  show Empty = "L"
  show Taken = "#"

type Point2D = (Int, Int)

type Direction = (Int, Int)

directions :: [Direction]
directions = [(a, b) | a <- [-1 .. 1], b <- [-1 .. 1], a /= 0 || b /= 0]

type Map2D a = IntMap (IntMap a)

bounds :: Map2D a -> ((Int, Int), (Int, Int))
bounds m = ((0, maximum $ map (maybe 0 fst . Map.lookupMax) $ Map.elems m), (0, maybe 0 fst $ Map.lookupMax m))

lookup2D :: Point2D -> Map2D a -> Maybe a
lookup2D (x, y) = Map.lookup y >=> Map.lookup x

mapWithKey2D :: (Point2D -> a -> b) -> Map2D a -> Map2D b
mapWithKey2D f = Map.mapWithKey (\y -> Map.mapWithKey (\x -> f (x, y)))

type SeatMap = Map2D Space

showSeatMap :: SeatMap -> String
showSeatMap m = unlines $ map concat [[maybe "_" show (lookup2D (x, y) m) | x <- [minX .. maxX]] | y <- [minY .. maxY]]
  where
    ((minX, maxX), (minY, maxY)) = bounds m

parse :: String -> SeatMap
parse = Map.fromList . zip [0 ..] . map parseRow . lines
  where
    parseRow = Map.fromList . zip [0 ..] . map readSpace

example =
  unlines
    [ "L.LL.LL.LL",
      "LLLLLLL.LL",
      "L.L.L..L..",
      "LLLL.LL.LL",
      "L.LL.LL.LL",
      "L.LLLLL.LL",
      "..L.L.....",
      "LLLLLLLLLL",
      "L.LLLLLL.L",
      "L.LLLLL.LL"
    ]

type GetSeats = SeatMap -> Point2D -> [Space]

visibleSeat :: SeatMap -> Point2D -> Direction -> Space
visibleSeat m (x, y) d@(a, b) = case lookup2D (x + a, y + b) m of
  Just Floor -> visibleSeat m (x + a, y + b) d
  Just x -> x
  Nothing -> Floor

adjacentSeat :: SeatMap -> Point2D -> Direction -> Space
adjacentSeat m (x, y) (a, b) = fromMaybe Floor (lookup2D (x + a, y + b) m)

visibleSeats :: SeatMap -> Point2D -> [Space]
visibleSeats m p = map (visibleSeat m p) directions

adjacentSeats :: SeatMap -> Point2D -> [Space]
adjacentSeats m p = map (adjacentSeat m p) directions

countTakenSeats :: GetSeats -> SeatMap -> Point2D -> Int
countTakenSeats f m p = length $ filter (== Taken) $ f m p

step :: GetSeats -> Int -> SeatMap -> SeatMap
step f tolerance m = mapWithKey2D stepLocation m
  where
    stepLocation p s
      | s == Empty = if countTakenSeats f m p == 0 then Taken else Empty
      | s == Taken = if countTakenSeats f m p >= tolerance then Empty else Taken
      | otherwise = s

step1 :: SeatMap -> SeatMap
step1 = step adjacentSeats 4

step2 :: SeatMap -> SeatMap
step2 = step visibleSeats 5

getStable :: (SeatMap -> SeatMap) -> SeatMap -> SeatMap
getStable step = f
  where
    f m = if showSeatMap y == showSeatMap m then m else f y
      where
        y = step m

countTaken :: SeatMap -> Int
countTaken = sum . map (Map.size . Map.filter (== Taken)) . Map.elems

part1 = countTaken . getStable step1

part2 = countTaken . getStable step2

main = interact $ show . part2 . parse
