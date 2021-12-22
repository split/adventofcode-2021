module Main where

import Data.Char (digitToInt)
import Data.Foldable
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

main = interact (unlines . sequence [part1] . map (player . last) . lines)

type Player = (Int, Int)

type Turn = (Player, Player, Int)

part1 :: [(Int, Int)] -> String
part1 = ("Part 1: " ++) . show . play . startTurn

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

rotating n value = ((value - 1) `rem` n) + 1

player s = (digitToInt s, 0)

startTurn (p1 : p2 : _) = (p1, p2, 0)

-- >>> chances
-- fromList [(3,1),(4,3),(5,6),(6,7),(7,6),(8,3),(9,1)]
chances :: Set (Int, Int)
chances = let c = [1, 2, 3] in S.fromList $ M.toList $ M.fromListWith (+) [(t1 + t2 + t3, 1) | t1 <- c, t2 <- c, t3 <- c]

hasWinner :: Int -> Turn -> Bool
hasWinner target = (>= target) . maximum . scores

scores :: Turn -> [Int]
scores ((_, s1), (_, s2), _) = [s1, s2]

throwCount :: Turn -> Int
throwCount (_, _, throw) = throw
