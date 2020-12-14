import Data.Char
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map

padLeft :: Int -> Char -> String -> String
padLeft n c s = replicate (n - length s) c ++ s

data MaskValue = Zero | One | Floating
  deriving (Show)

readMask :: Char -> MaskValue
readMask '0' = Zero
readMask '1' = One
readMask 'X' = Floating

applyMaskChar :: Char -> MaskValue -> Char
applyMaskChar _ Zero = '0'
applyMaskChar _ One = '1'
applyMaskChar c _ = c

applyMask :: String -> [MaskValue] -> String
applyMask s ms = zipWith applyMaskChar (padLeft (length ms) '0' s) ms

stringToInt :: String -> Integer
stringToInt = foldl (\b a -> 2 * b + a) 0 . map (toInteger . digitToInt)

intToString :: Integer -> String
intToString 0 = []
intToString n = intToString q ++ [intToDigit $ fromInteger r]
  where
    (q, r) = n `divMod` 2

type Memory = Map Integer Integer

initialMemory :: Memory
initialMemory = Map.empty

updateMemory :: [MaskValue] -> Integer -> String -> Memory -> Memory
updateMemory mask i input = Map.insert i (stringToInt $ applyMask input mask)

data Instruction = SetMask [MaskValue] | UpdateMemory Integer String
  deriving (Show)

type Machine = ([MaskValue], Memory)

initialMachine :: Machine
initialMachine = (replicate 36 Floating, initialMemory)

step :: Instruction -> Machine -> Machine
step (SetMask m) (_, mem) = (m, mem)
step (UpdateMemory v s) (m, mem) = (m, updateMemory m v s mem)

parseLine :: String -> Instruction
parseLine xs = if y == "mask" then SetMask (map readMask ys) else UpdateMemory (read z) (intToString $ read ys)
  where
    [y, ys] = splitOn " = " xs
    z = takeWhile isDigit $ dropWhile (not . isDigit) y

getTotal :: Machine -> Integer
getTotal = sum . Map.elems . snd

part1 :: [Instruction] -> Integer
part1 = getTotal . foldl (flip step) initialMachine

applyMaskChar2 :: Char -> MaskValue -> MaskValue
applyMaskChar2 c Zero = readMask c
applyMaskChar2 _ One = One
applyMaskChar2 _ Floating = Floating

applyMask2 :: String -> [MaskValue] -> [MaskValue]
applyMask2 s ms = zipWith applyMaskChar2 (padLeft (length ms) '0' s) ms

getValues :: [MaskValue] -> [Integer]
getValues = f 0
  where
    f acc [] = [acc]
    f acc (Floating : xs) = f (2 * acc) xs ++ f (2 * acc + 1) xs
    f acc (Zero : xs) = f (2 * acc) xs
    f acc (One : xs) = f (2 * acc + 1) xs

updateMemory2 :: [MaskValue] -> String -> Integer -> Memory -> Memory
updateMemory2 mask i input mem = foldl (\m x -> Map.insert x input m) mem $ getValues (applyMask2 i mask)

step2 :: Instruction -> Machine -> Machine
step2 (SetMask m) (_, mem) = (m, mem)
step2 (UpdateMemory v s) (m, mem) = (m, updateMemory2 m (intToString v) (stringToInt s) mem)

part2 :: [Instruction] -> Integer
part2 = getTotal . foldl (flip step2) initialMachine

main = interact $ show . part2 . map parseLine . lines