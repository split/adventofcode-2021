import Data.Char (digitToInt)
import Data.List (foldl', group, sort, sortOn, transpose)

main = interact (part1 . lines)

part1 :: [String] -> String
part1 = (++) "Part 1: " <$> show . product . map toDec . gammaAndEpsilon

gammaAndEpsilon :: [[Char]] -> [[Char]]
gammaAndEpsilon = transpose . map (map head . sortOn length . group . sort . (++) "01") . transpose

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0