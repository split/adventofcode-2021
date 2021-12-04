{-# LANGUAGE TupleSections #-}

import Control.Monad (ap)
import Data.List (find, transpose)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)

type Card = [[Int]]

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
    markDrawn cards n = map (map (map (\x -> if x == n then - 1 else x))) cards
    isWinner = elem 0 . map (length . filter (>= 0)) . ap (++) transpose
    score (card, n) = n * sum (filter (> 0) $ concat card)

parseInput :: String -> ([Card], Drawn)
parseInput = parse . splitOn "\n\n"
  where
    parse (drawn : cards) = (map parseCard cards, read <$> splitOn "," drawn)
    parseCard = map (map read . words) . lines
