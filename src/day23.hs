import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad
import Control.Monad.ST
import qualified Data.Map as Map
import Data.Vector.Unboxed.Mutable (MVector)
import qualified Data.Vector.Unboxed.Mutable as MVector
import Data.List (intercalate)

type Cups s = MVector s Int

parse :: PrimMonad m => ([Int] -> [Int]) -> String -> m (Cups (PrimState m))
parse f s = toVec . Map.fromList . zip cs $ tail cs ++ cs
    where
        cs = f $ map (read . (:[])) s
        toVec m = MVector.generate (1 + Map.size m) (\i -> if i == 0 then head cs else m Map.! i)

getDest :: Int -> Int -> [Int] -> Int
getDest max n xs = go (n-1)
    where
        go 0 = go max
        go n = if n `elem` xs then go (n-1) else n

move :: Cups s -> ST s ()
move cs = do
    c <- MVector.read cs 0
    x <- MVector.read cs c
    y <- MVector.read cs x
    z <- MVector.read cs y
    a <- MVector.read cs z
    let mx = MVector.length cs - 1
    let d = getDest mx c [x,y,z]
    e <- MVector.read cs d
    MVector.write cs c a
    MVector.write cs d x
    MVector.write cs z e
    MVector.write cs 0 a

getFollowing :: PrimMonad m => Int -> Int -> Cups (PrimState m) -> m [Int]
getFollowing i n m = if n == 0 then
        return []
    else do
        x <- MVector.read m i
        xs <- getFollowing x (n-1) m
        return (x:xs)

getFollowingOne :: PrimMonad m => Int -> Cups (PrimState m) -> m [Int]
getFollowingOne = getFollowing 1

iterateN :: Int -> (Cups s -> ST s b) -> Cups s -> ST s ()
iterateN n f v = do
    forM_ [1..n] $ \_ ->
        f v

part1 :: String
part1 = runST $ do
    v <- parse id "716892543"
    iterateN 100 move v
    xs <- getFollowingOne 8 v
    return . intercalate  "" $ map show xs
    

part2 :: String
part2 = runST $ do
    v <- parse (++[10..1000000]) "716892543"
    iterateN 10000000 move v
    xs <- getFollowingOne 2 v
    return . show $ product xs

main = print part2