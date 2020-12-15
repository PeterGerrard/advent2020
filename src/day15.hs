import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

type History = IntMap Int

getNext :: IntMap Int -> Int -> Int -> Int
getNext h p t = case IntMap.lookup p h of
  Nothing -> 0
  Just n -> t - n

continueGame :: Int -> IntMap Int -> Int -> Int -> Int
continueGame 1 _ p _ = p
continueGame x h p t = continueGame (x -1) (IntMap.insert p t h) next (t + 1)
  where
    next = getNext h p t

startGame :: Int -> [Int] -> Int
startGame x is = continueGame (x - n) h (last is) n
  where
    h = IntMap.fromList $ zip (take n is) [0 ..]
    n = length is - 1

input :: [Int]
input = [1, 17, 0, 10, 18, 11, 6]

part1 = startGame 2020 input

part2 = startGame 30000000 input

examples :: [[Int]]
examples = [[0, 3, 6], [1, 3, 2], [2, 1, 3], [1, 2, 3], [2, 3, 1], [3, 2, 1], [3, 1, 2]]

main = putStr $ show part2