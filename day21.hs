{-# LANGUAGE TupleSections #-}

module Main where

import Data.Char (digitToInt)
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as M

type Player = (Int, Int)

type Wins = (Int, Int)

type Turn = (Player, Player, Int)

type Multiverse = Map (Player, Player) Wins

main = interact (unlines . sequence [part1, part2] . map (player . last) . lines)

part1, part2 :: [(Int, Int)] -> String
part1 = ("Part 1: " ++) . show . play . startTurnCounter
part2 = ("Part 2: " ++) . show . uncurry max . snd . dirac M.empty . startTurn

play :: Turn -> Int
play = maybe 0 looser . find (hasWinner 1000) . iterate turn
  where
    looser r = throwCount r * minimum (scores r)

turn :: Turn -> Turn
turn ((pos, totalScore), next, throws) =
  let score = roll3 throws
      pos' = rotating 10 (pos + score)
   in (next, (pos', totalScore + pos'), throws + 3)

roll3 :: Int -> Int
roll3 throw = let roll = rotating 100 . (throw +) in roll 1 + roll 2 + roll 3

dirac :: Multiverse -> (Player, Player) -> (Multiverse, Wins)
dirac multiverse turn@((p1, s1), p2s@(_, s2))
  | s1 >= 21 = (multiverse, (1, 0))
  | s2 >= 21 = (multiverse, (0, 1))
  | otherwise = case multiverse M.!? turn of
    Just wins -> (multiverse, wins)
    Nothing ->
      let (multiverse', wins) = foldl go (multiverse, (0, 0)) chances
       in (M.insert turn wins multiverse', wins)
  where
    go (multiverse, (w1, w2)) (score, count) =
      let pos' = rotating 10 (p1 + score)
          (multiverse', (w2', w1')) = dirac multiverse (p2s, (pos', s1 + pos'))
       in (multiverse', (w1 + count * w1', w2 + count * w2'))

rotating :: Integral a => a -> a -> a
rotating n value = ((value - 1) `rem` n) + 1

player s = (digitToInt s, 0)

startTurn (p1 : p2 : _) = (p1, p2)

startTurnCounter (p1 : p2 : _) = (p1, p2, 0)

-- >>> chances
-- fromList [(3,1),(4,3),(5,6),(6,7),(7,6),(8,3),(9,1)]
chances :: [(Int, Int)]
chances = let c = [1, 2, 3] in M.toList $ M.fromListWith (+) [(t1 + t2 + t3, 1) | t1 <- c, t2 <- c, t3 <- c]

hasWinner :: Int -> Turn -> Bool
hasWinner target = (>= target) . maximum . scores

scores :: Turn -> [Int]
scores ((_, s1), (_, s2), _) = [s1, s2]

throwCount :: Turn -> Int
throwCount (_, _, throw) = throw
