import Control.Arrow
import Data.List
import Data.List.Split

newtype BusId = Bus Integer
  deriving (Show)

newtype Timestamp = Time Integer
  deriving (Show, Eq, Ord, Read)

getNextTime :: Timestamp -> BusId -> Timestamp
getNextTime (Time t) (Bus i) = Time $ i * ceiling (fromInteger t / fromInteger i)

parseIds :: String -> [BusId]
parseIds = map (Bus . read) . filter (/= "x") . splitOn ","

answerValue :: Timestamp -> BusId -> Timestamp -> Integer
answerValue (Time s) (Bus b) (Time t) = (t - s) * b

part1 :: Timestamp -> [BusId] -> Integer
part1 t = uncurry (answerValue t) . minimumBy (\(_, t1) (_, t2) -> compare t1 t2) . map (\x -> (x, getNextTime t x))

parse :: String -> (Timestamp, [BusId])
parse s = (Time $ read t, parseIds bs)
  where
    [t, bs] = lines s

parseIdsWithOffset :: String -> [CRT]
parseIdsWithOffset = map (second read) . filter ((/= "x") . snd) . zip (map (0 -) [0 ..]) . splitOn ","

computeCoeff a b = go a b 1 0 0 1
  where
    go a b s0 s1 t0 t1
      | r == 0 = (s1, t1)
      | otherwise = go b r s1 s t1 t
      where
        (q, r) = quotRem a b
        (s, t) = (s0 - s1 * q, t0 - t1 * q)

type CRT = (Integer, Integer)

mergeCRT :: CRT -> CRT -> CRT
mergeCRT (a1, n1) (a2, n2) = (x `mod` n, n)
  where
    (m1, m2) = computeCoeff n1 n2
    x = (a1 * m2 * n2) + (a2 * m1 * n1)
    n = n1 * n2

mergeCRTs :: [CRT] -> CRT
mergeCRTs = foldl1 mergeCRT

part2 :: [CRT] -> Integer
part2 = uncurry mod . mergeCRTs

main = interact $ show . part2 . parseIdsWithOffset . (!! 1) . lines