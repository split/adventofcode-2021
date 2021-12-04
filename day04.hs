{-# LANGUAGE TupleSections #-}

import Control.Monad (ap)
import Data.List (find, transpose)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)

type Card = [[(Int, Bool)]]

type Drawn = [Int]

main :: IO ()
main = interact (unlines . sequence [part1, part2] . uncurry playBingo . parseInput)

part1, part2 :: [Int] -> String
part1 = ("Part 1: " ++) . show . head
part2 = ("Part 2: " ++) . show . last

playBingo :: [Card] -> Drawn -> [Int]
playBingo cards drawn = map score $ catMaybes $ zipWith (\round n -> (,n) <$> find isWinner round) rounds drawn
  where
    rounds = drop 1 $ scanl (markDrawn . filter (not . isWinner)) cards drawn
    markDrawn cards n = map (map (map (\(n', b) -> (n', b || n' == n)))) cards
    isWinner = any (all snd) . ap (++) transpose
    score (card, n) = n * sum [n' | (n', b) <- concat card, not b]

parseInput :: String -> ([Card], Drawn)
parseInput = parse . splitOn "\n\n"
  where
    parse (drawn : cards) = (map parseCard cards, read <$> splitOn "," drawn)
    parseCard = map (map ((,False) . read) . words) . lines
