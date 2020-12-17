import Control.Monad
import Data.IntMap (IntMap)
import qualified Data.IntMap as Map

type Point3D = (Int, Int, Int)

type Point4D = (Int, Int, Int, Int)

type Direction3D = (Int, Int, Int)

type Direction4D = (Int, Int, Int, Int)

directions3D :: [Direction3D]
directions3D = [(a, b, c) | a <- [-1 .. 1], b <- [-1 .. 1], c <- [-1 .. 1], a /= 0 || b /= 0 || c /= 0]

adjacent3D :: Point3D -> [Point3D]
adjacent3D (x, y, z) = [(x + a, y + b, z + c) | (a, b, c) <- directions3D]

directions4D :: [Direction4D]
directions4D = [(a, b, c, d) | a <- [-1 .. 1], b <- [-1 .. 1], c <- [-1 .. 1], d <- [-1 .. 1], a /= 0 || b /= 0 || c /= 0 || d /= 0]

adjacent4D :: Point4D -> [Point4D]
adjacent4D (x, y, z, w) = [(x + a, y + b, z + c, w + d) | (a, b, c, d) <- directions4D]

type Map3D a = IntMap (IntMap (IntMap a))

type Map4D a = IntMap (Map3D a)

minKey :: IntMap a -> Int
minKey = maybe 0 fst . Map.lookupMin

maxKey :: IntMap a -> Int
maxKey = maybe 0 fst . Map.lookupMax

bounds3D :: Map3D a -> ((Int, Int), (Int, Int), (Int, Int))
bounds3D m =
  ( (minimum $ map minKey $ concatMap Map.elems $ Map.elems m, maximum $ map maxKey $ concatMap Map.elems $ Map.elems m),
    (minimum $ map minKey $ Map.elems m, maximum $ map maxKey $ Map.elems m),
    (minKey m, maxKey m)
  )

lookup3D :: Point3D -> Map3D a -> Maybe a
lookup3D (x, y, z) = Map.lookup z >=> Map.lookup y >=> Map.lookup x

mapWithKey3D :: (Point3D -> a -> b) -> Map3D a -> Map3D b
mapWithKey3D f = Map.mapWithKey (\z -> Map.mapWithKey (\y -> Map.mapWithKey (\x -> f (x, y, z))))

insert3D :: Point3D -> a -> Map3D a -> Map3D a
insert3D (x, y, z) v = Map.insertWith update2 z single2
  where
    update2 :: IntMap (IntMap a) -> IntMap (IntMap a) -> IntMap (IntMap a)
    update2 = Map.unionWith update1
    update1 :: IntMap a -> IntMap a -> IntMap a
    update1 = Map.unionWith (curry snd)
    single2 = Map.fromList [(y, Map.fromList [(x, v)])]

from3DList :: [(Point3D, a)] -> Map3D a
from3DList = foldl (flip (uncurry insert3D)) Map.empty

bounds4D :: Map4D a -> ((Int, Int), (Int, Int), (Int, Int), (Int, Int))
bounds4D m =
  ( (minimum $ map minKey $ concatMap Map.elems $ concatMap Map.elems $ Map.elems m, maximum $ map maxKey $ concatMap Map.elems $ concatMap Map.elems $ Map.elems m),
    (minimum $ map minKey $ concatMap Map.elems $ Map.elems m, maximum $ map maxKey $ concatMap Map.elems $ Map.elems m),
    (minimum $ map minKey $ Map.elems m, maximum $ map maxKey $ Map.elems m),
    (minKey m, maxKey m)
  )

lookup4D :: Point4D -> Map4D a -> Maybe a
lookup4D (x, y, z, w) = Map.lookup w >=> Map.lookup z >=> Map.lookup y >=> Map.lookup x

mapWithKey4D :: (Point4D -> a -> b) -> Map4D a -> Map4D b
mapWithKey4D f = Map.mapWithKey (\w -> Map.mapWithKey (\z -> Map.mapWithKey (\y -> Map.mapWithKey (\x -> f (x, y, z, w)))))

insert4D :: Point4D -> a -> Map4D a -> Map4D a
insert4D (x, y, z, w) v = Map.insertWith update3 w single3
  where
    update3 = Map.unionWith update2
    update2 = Map.unionWith update1
    update1 = Map.unionWith (curry snd)
    single3 = Map.fromList [(z, Map.fromList [(y, Map.fromList [(x, v)])])]

from4DList :: [(Point4D, a)] -> Map4D a
from4DList = foldl (flip (uncurry insert4D)) Map.empty

type ConwayCube3 = Map3D Bool

type ConwayCube4 = Map4D Bool

countAdjacent3D :: ConwayCube3 -> Point3D -> Int
countAdjacent3D c = length . filter ((== Just True) . flip lookup3D c) . adjacent3D

countAdjacent4D :: ConwayCube4 -> Point4D -> Int
countAdjacent4D c = length . filter ((== Just True) . flip lookup4D c) . adjacent4D

step3D :: ConwayCube3 -> ConwayCube3
step3D c = from3DList $ filter snd $ map (\p -> (p, stepSingle p)) pointsToLookAt
  where
    ((minX, maxX), (minY, maxY), (minZ, maxZ)) = bounds3D c
    pointsToLookAt = [(x, y, z) | x <- [minX -1 .. maxX + 1], y <- [minY -1 .. maxY + 1], z <- [minZ -1 .. maxZ + 1]]
    stepSingle p
      | lookup3D p c == Just True = ns == 2 || ns == 3
      | otherwise = ns == 3
      where
        ns = countAdjacent3D c p

step4D :: ConwayCube4 -> ConwayCube4
step4D c = from4DList $ filter snd $ map (\p -> (p, stepSingle p)) pointsToLookAt
  where
    ((minX, maxX), (minY, maxY), (minZ, maxZ), (minW, maxW)) = bounds4D c
    pointsToLookAt = [(x, y, z, w) | x <- [minX -1 .. maxX + 1], y <- [minY -1 .. maxY + 1], z <- [minZ -1 .. maxZ + 1], w <- [minW -1 .. maxW + 1]]
    stepSingle p
      | lookup4D p c == Just True = ns == 2 || ns == 3
      | otherwise = ns == 3
      where
        ns = countAdjacent4D c p

printCube3 :: ConwayCube3 -> String
printCube3 c = unlines [printLayer z | z <- [minZ .. maxZ]]
  where
    ((minX, maxX), (minY, maxY), (minZ, maxZ)) = bounds3D c
    printLayer z = "z = " ++ show z ++ "\n" ++ unlines [[if lookup3D (x, y, z) c == Just True then '#' else '.' | x <- [minX .. maxX]] | y <- [minY .. maxY]]

printCube4 :: ConwayCube4 -> String
printCube4 c = unlines [printLayer z w | w <- [minW .. maxW], z <- [minZ .. maxZ]]
  where
    ((minX, maxX), (minY, maxY), (minZ, maxZ), (minW, maxW)) = bounds4D c
    printLayer z w = "z=" ++ show z ++ ", w=" ++ show w ++ "\n" ++ unlines [[if lookup4D (x, y, z, w) c == Just True then '#' else '.' | x <- [minX .. maxX]] | y <- [minY .. maxY]]

parseLayer3 :: Int -> String -> ConwayCube3
parseLayer3 z s = Map.fromList [(z, parseLayer s)]
  where
    parseLayer = Map.fromList . zip [0 ..] . map parseRow . lines
    parseRow = Map.fromList . zip [0 ..] . map (== '#')

parseLayer4 :: Int -> Int -> String -> ConwayCube4
parseLayer4 z w s = Map.fromList [(w, parseLayer3 z s)]

countActive3 :: ConwayCube3 -> Int
countActive3 = length . filter (== True) . concatMap (concatMap Map.elems . Map.elems) . Map.elems

countActive4 :: ConwayCube4 -> Int
countActive4 = length . filter (== True) . concatMap (concatMap Map.elems . concatMap Map.elems . Map.elems) . Map.elems

doN :: Int -> (a -> a) -> a -> a
doN 0 _ = id
doN n f = doN (n -1) f . f

part1 :: ConwayCube3 -> Int
part1 = countActive3 . doN 6 step3D

part2 :: ConwayCube4 -> Int
part2 = countActive4 . doN 6 step4D

main = interact $ show . part2 . parseLayer4 0 0