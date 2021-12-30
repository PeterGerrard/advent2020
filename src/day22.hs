{-# LANGUAGE TupleSections #-}

import Control.Arrow
import Data.List.Split
import Data.Sequence (Seq, (|>), ViewL((:<)), ViewR((:>)))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Foldable as Seq

type Deck = Seq Int

turn :: (Deck,Deck) -> (Deck,Deck)
turn (d1,d2) = case (Seq.viewl d1, Seq.viewl d2) of
    (Seq.EmptyL, _) -> (d1,d2)
    (_, Seq.EmptyL) -> (d1,d2)
    (x :< xs, y :< ys) -> if x<y then (xs,ys |> y |> x) else (xs |> x |> y,ys)

getWinningDeck :: (Deck,Deck) -> Deck
getWinningDeck = (\(d1,d2) -> if null d1 then d2 else d1) . head . dropWhile (\(d1,d2) -> not (null d1 || null d2)) . iterate turn

score :: Deck -> Int
score = sum . zipWith (*) [1..] . reverse . Seq.toList

both :: (a -> b) -> (a,a) -> (b,b)
both f (x,y) = (f x, f y)

parse :: String -> (Deck,Deck)
parse s = both (Seq.fromList . map read . drop 1 . lines) (p1,p2)
    where [p1,p2] = splitOn "\n\n" s

part1 = show . score . getWinningDeck

recCombatTurn :: (Deck,Deck) -> (Deck,Deck)
recCombatTurn d@(ds1,ds2) = case (Seq.viewl ds1, Seq.viewl ds2) of
        (Seq.EmptyL, _) -> d
        (_, Seq.EmptyL) -> d
        (x :< xs, y :< ys) -> if length xs >= x && length ys >= y then
                                case recCombat Set.empty (Seq.take x xs, Seq.take y ys) of
                                    (seen'', Left _) -> (xs |> x |> y, ys)
                                    (seen'', Right _) -> (xs, ys |> y |> x)
                              else if x > y then
                                (xs |> x |> y, ys)
                              else
                                (xs, ys |> y |> x)

toWinner :: (Deck, Deck) -> Either Deck Deck
toWinner (d1,d2) = case (Seq.viewl d1, Seq.viewl d2) of
    (Seq.EmptyL, _) -> Right d2
    (_, Seq.EmptyL) -> Left d1
    _ -> error "Nope"

recCombat :: Set (Deck,Deck) -> (Deck,Deck) -> (Set (Deck,Deck), Either Deck Deck)
recCombat seen = second toWinner . head . dropWhile (\(_, (d1,d2)) -> not (null d1 || null d2)) . iterate (\(s,d) -> if d `Set.member` s then (s, second (const Seq.empty) d) else (Set.insert d s, recCombatTurn d)) . (seen,)

part2 = show . score . getDeck . snd . recCombat Set.empty
    where
        getDeck (Left x) = x
        getDeck (Right x) = x

main = interact $ part2 . parse