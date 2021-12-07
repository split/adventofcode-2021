import Control.Monad (ap)
import Data.List
import Data.List.Split

main :: IO ()
main = interact (unlines . sequence [part1, part2] . map read . splitOn ",")

part1, part2 :: [Int] -> [Char]
part1 = ("Part 1: " ++) . show . ap (flip $ sumCost id) median
part2 = ("Part 2: " ++) . show . ap (best . flip (sumCost increasingCost)) mean
  where
    best f m = minimum $ map f [floor m, ceiling m]
    increasingCost x = x * (x + 1) `div` 2

sumCost :: (Int -> Int) -> Int -> [Int] -> Int
sumCost cost to = sum . map (cost . abs . subtract to)

median :: [Int] -> Int
median xs = sort xs !! (length xs `div` 2 - 1)

mean :: (Fractional b, Real a) => [a] -> b
mean xs = realToFrac (sum xs) / genericLength xs