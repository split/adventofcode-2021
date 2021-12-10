import Data.Either
import Data.List (elemIndex, sort)
import Data.Maybe (mapMaybe)

main :: IO ()
main = interact (unlines . sequence [part1, part2] . map (parse "") . lines)

part1, part2 :: [Either Char String] -> [Char]
part1 = ("Part 1: " ++) . show . sum . mapMaybe (`lookup` scores) . lefts
part2 = ("Part 2: " ++) . show . middle . sort . map sumScore . rights

parse :: String -> String -> Either Char String
parse s ('(' : ix) = parse (')' : s) ix
parse s ('[' : ix) = parse (']' : s) ix
parse s ('{' : ix) = parse ('}' : s) ix
parse s ('<' : ix) = parse ('>' : s) ix
parse s "" = Right s
parse (x : xs) (i : ix)
  | x == i = parse xs ix
  | otherwise = Left i

scores :: [(Char, Int)]
scores = [(')', 3), (']', 57), ('}', 1197), ('>', 25137)]

sumScore :: String -> Int
sumScore = foldl (\p v -> 5 * p + v + 1) 0 . mapMaybe (`elemIndex` map fst scores)

middle :: [a] -> a
middle l@(_ : _ : _ : _) = middle $ tail $ init l
middle [x] = x