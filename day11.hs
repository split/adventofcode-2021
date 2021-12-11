import Data.Char (digitToInt)
import Data.Foldable (Foldable (foldr'))
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S

type Point = (Int, Int)

type Grid a = Map Point a

main = interact (unlines . sequence [part1, part2] . grid . lines)

part1, part2 :: Grid Int -> String
part1 = ("Part 1: " ++) . show . sum . map (M.size . M.filter (== 0)) . take 101 . iterate simulate
part2 = ("Part 2: " ++) . show . length . takeWhile (any (> 0)) . iterate simulate

simulate :: Grid Int -> Grid Int
simulate = flash . M.map (+ 1)

flash :: Grid Int -> Grid Int
flash grid
  | S.null pointsToFlash = grid
  | otherwise = flash $ update grid
  where
    pointsToFlash = M.keysSet $ M.filter (> 9) grid
    receivesEnergy = concatMap adjacent pointsToFlash
    update = flashPoints pointsToFlash . transmitEnergy receivesEnergy
    flashPoints = flip (foldr' (`M.insert` 0))
    transmitEnergy = flip (foldr' (M.adjust (\v -> if v > 0 then v + 1 else 0)))

grid :: [String] -> Grid Int
grid rows = M.fromList [((y, x), digitToInt col) | (cols, y) <- zip rows [0 ..], (col, x) <- zip cols [0 ..]]

adjacent (x, y) = [(x + dx, y + dy) | dx <- [-1 .. 1], dy <- [-1 .. 1], (dx, dy) /= (0, 0)]