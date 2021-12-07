import Control.Monad (ap)
import Data.List
import Data.List.Split

main :: IO ()
main = interact (unlines . sequence [part1, part2] . sort . map read . splitOn ",")

part1, part2 :: [Int] -> [Char]
part1 = ("Part 1: " ++) . show . ap (flip $ sumCost id) median
part2 = ("Part 2: " ++) . show . ap (flip $ sumCost increasingCost) mean
  where
    increasingCost x = x * (x + 1) `div` 2

sumCost :: (Int -> Int) -> Int -> [Int] -> Int
sumCost cost to = sum . map (cost . abs . subtract to)

median, mean :: [Int] -> Int
median xs = xs !! (length xs `div` 2 - 1)
mean xs = round (value + (count - 2 * countLessThan value) / (2 * count))
  where
    value = realToFrac (sum xs) / count
    countLessThan x = genericLength $ takeWhile ((< x) . fromIntegral) xs
    count = genericLength xs