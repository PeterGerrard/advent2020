{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TupleSections #-}
import Data.List (intercalate, partition)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

data Dir = E | SE | SW | W | NW | NE

instance Read Dir where
    readsPrec _ ('e':xs) = [(E,xs)]
    readsPrec _ ('s':'e':xs) = [(SE,xs)]
    readsPrec _ ('s':'w':xs) = [(SW,xs)]
    readsPrec _ ('w':xs) = [(W,xs)]
    readsPrec _ ('n':'e':xs) = [(NE,xs)]
    readsPrec _ ('n':'w':xs) = [(NW,xs)]
    readsPrec _ _ = []

    readList s = [(go s,"")]
        where
            go :: String -> [Dir]
            go [] = []
            go xs = concatMap (\(d,ys) -> d : go ys) $ reads xs

instance Show Dir where
    show E = "e"
    show SE = "se"
    show SW = "sw"
    show W = "w"
    show NW = "nw"
    show NE = "ne"

    showList ds = (++) (intercalate "" $ map show ds)

type Position = (Int, Int)

-- -1   0   1
--    0   1
-- -1   0   1

move :: Position -> Dir -> Position
move (x,y) E = (x+1,y)
move (x,y) SE = (x+1,y+1)
move (x,y) SW = (x,y+1)
move (x,y) W = (x-1,y)
move (x,y) NW = (x-1,y-1)
move (x,y) NE = (x,y-1)

flipTile :: Set Position -> [Dir] -> Set Position
flipTile flipped path = update p' flipped
    where
        p' = foldl move (0,0) path
        update = if Set.member p' flipped then Set.delete else Set.insert

flipTiles :: Set Position -> [[Dir]] -> Set Position
flipTiles = foldl flipTile

part1 :: [[Dir]] -> String
part1 = show . Set.size . flipTiles Set.empty

gameOfLife :: Set Position -> Set Position
gameOfLife s = Set.fromList (livingBlacks ++ livingWhites)
    where
        m = Map.fromListWith (+) $ map (,1) $ concatMap adjacent (Set.toList s)
        (blacks,whites) = partition (`Set.member` s) $ Map.keys m
        livingBlacks = filter (\p -> m Map.! p == 1 || m Map.! p == 2) blacks
        livingWhites = filter (\p -> m Map.! p == 2) whites

adjacent :: Position -> [Position]
adjacent p = map (move p) [E,SE,SW,W,NW,NE]

part2 :: [[Dir]] -> String
part2 = show . Set.size . (!!100) . iterate gameOfLife . flipTiles Set.empty

main = interact $ part2 . map read . lines