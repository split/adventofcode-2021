import Data.List
import Data.List.Split
import Data.Set (Set)
import qualified Data.Set as S

type Point = (Int, Int)

data Line = Line Point Point deriving (Eq, Show)

main :: IO ()
main = interact (unlines . sequence [part1, part2] . map parse . lines)

part1, part2 :: [Line] -> [Char]
part1 = ("Part 1: " ++) . show . S.size . overlap . map points . filter isDiagonal
part2 = ("Part 2: " ++) . show . S.size . overlap . map points

parse :: String -> Line
parse = toLine . map (toPoint . map read . splitOn ",") . splitOn " -> "
  where
    toPoint [x, y] = (x, y)
    toLine [start, end] = Line start end

overlap :: Ord a => [Set a] -> Set a
overlap lines = S.unions [S.intersection p1 p2 | (p1 : ps) <- tails lines, p2 <- ps]

points :: Line -> Set Point
points (Line (x1, y1) (x2, y2)) = S.fromList $ zip (range x1 x2) (range y1 y2)
  where
    range a b = [a, a + signum (b - a) .. b]

isDiagonal :: Line -> Bool
isDiagonal (Line (x1, y1) (x2, y2)) = x1 == x2 || y1 == y2
