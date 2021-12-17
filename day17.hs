import Control.Monad (ap)
import Data.List (tails, unfoldr)
import Data.Maybe (listToMaybe)

main = interact (unlines . sequence [part1, part2] . readArea)

part1, part2 :: [Int] -> String
part1 [_, _, minY, _] = "Part 1: " ++ show (minY * (minY + 1) `div` 2)
part2 = ("Part 2: " ++) . show . length . filter id . ap (map . flip check) limits

limits [_, maxX, _, _] = [(dx, dy) | dx <- [1 .. maxX + 1], dy <- [-1000 .. 1000]]

check :: (Int, Int) -> [Int] -> Bool
check vel [minX, maxX, minY, maxY] = any isInside $ takeWhile isPossible $ iterate step ((0, 0), vel)
  where
    isInside ((x, y), _) = x >= minX && x <= maxX && y >= minY && y <= maxY
    isPossible ((x, y), (_, _)) = y >= minY && x < maxX * 2

step (pos, vel@(dx, _)) = (pos `add` vel, vel `add` (- signum dx, - 1))

add (x, y) (x', y') = (x + x', y + y')

readArea = unfoldr $ listToMaybe . concatMap reads . tails