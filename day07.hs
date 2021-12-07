import Data.List.Split

main :: IO ()
main = interact (unlines . sequence [part1, part2] . map read . splitOn ",")

part1, part2 :: [Int] -> [Char]
part1 = ("Part 1: " ++) . show . minCost id
part2 = ("Part 2: " ++) . show . minCost increasingCost
  where
    increasingCost x = x * (x + 1) `div` 2

minCost :: (Int -> Int) -> [Int] -> Int
minCost cost xs = minimum $ map sum $ [map (cost . abs . subtract a) xs | a <- xs]