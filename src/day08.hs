import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

type Accumulator = Integer

type InstructionPointer = Integer

data Console = Console InstructionPointer Accumulator

initialConsole = Console 0 0

data Op
  = Acc Integer
  | Jmp Integer
  | Nop Integer
  deriving (Eq, Show)

parseOp :: String -> Op
parseOp s = case o of
  "acc" -> Acc i
  "jmp" -> Jmp i
  "nop" -> Nop i
  _ -> error $ "unknown op code " ++ o
  where
    [o, n] = words s
    i = case n of
      ('+' : ns) -> read ns
      _ -> read n

type InstructionsSet = Map InstructionPointer Op

step :: Console -> InstructionsSet -> Maybe Console
step (Console ip acc) is = case i of
  Just (Acc a) -> Just $ Console (ip + 1) (acc + a)
  Just (Jmp j) -> Just $ Console (ip + j) acc
  Just (Nop _) -> Just $ Console (ip + 1) acc
  Nothing -> Nothing
  where
    i = Map.lookup ip is

part1' :: Set InstructionPointer -> Console -> InstructionsSet -> Accumulator
part1' visited (Console ip acc) ops
  | ip `elem` visited = acc
  | otherwise = part1' (Set.insert ip visited) next ops
  where
    Just next = step (Console ip acc) ops

part1 :: InstructionsSet -> Accumulator
part1 = part1' Set.empty initialConsole

alternativeInstructions :: InstructionsSet -> [InstructionsSet]
alternativeInstructions is = f $ Map.toList is
  where
    f [] = []
    f ((i, Acc a) : xs) = map (Map.insert i (Acc a)) (f xs)
    f ((i, Jmp a) : xs) = Map.fromList ((i, Nop a) : xs) : map (Map.insert i (Jmp a)) (f xs)
    f ((i, Nop a) : xs) = Map.fromList ((i, Jmp a) : xs) : map (Map.insert i (Nop a)) (f xs)

part2' :: Set InstructionPointer -> Console -> InstructionsSet -> Maybe Accumulator
part2' visited (Console ip acc) ops
  | ip `elem` visited = Nothing
  | otherwise = case s of
    Just next -> part2' (Set.insert ip visited) next ops
    Nothing -> Just acc
  where
    s = step (Console ip acc) ops

part2 :: InstructionsSet -> Accumulator
part2 = single . map fromJust . filter isJust . map (part2' Set.empty initialConsole) . alternativeInstructions
  where
    single [x] = x

main = interact $ show . part2 . Map.fromList . zip [0 ..] . map parseOp . lines