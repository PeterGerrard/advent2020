import Data.List (elemIndex)
import Data.Maybe (fromJust)

step :: Integer -> Integer -> Integer
step subject n = (n * subject) `mod` 20201227

transform :: Integer -> [Integer]
transform subject = iterate (step subject) 1

iterateN :: Integer -> (a -> a) -> a -> a
iterateN 0 _ x = x
iterateN n f x = iterateN (n-1) f $! f x

transformN :: Integer -> Integer -> Integer
transformN n subject = iterateN n (step subject) 1

findLoopSize :: Integer -> Integer
findLoopSize n = toInteger . fromJust $ elemIndex n possible
    where
        possible = transform 7

--solve :: (Integer, Integer) -> 
solve (cardPublic, doorPublic) = transformN (findLoopSize cardPublic) doorPublic