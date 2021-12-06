import Data.List.Split

main :: IO ()
main = interact (unlines . sequence [part1, part2] . generations . map read . splitOn ",")

part1, part2 :: [Int] -> String
part1 = ("Part 1: " ++) . show . (!! 80)
part2 = ("Part 2: " ++) . show . (!! 256)

generations :: [Int] -> [Int]
generations = map sum . iterate simulate . counts
  where
    simulate [i0, i1, i2, i3, i4, i5, i6, i7, i8] = [i1, i2, i3, i4, i5, i6, i7 + i0, i8, i0]
    counts xs = [length $ filter (== i) xs | i <- [0 .. 8]]