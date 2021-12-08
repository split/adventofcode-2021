import Control.Monad (ap)
import Data.Foldable (foldl')
import Data.List (elemIndex)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S

main :: IO ()
main = interact (unlines . sequence [part1, part2] . map parse . lines)

part1, part2 :: [([Set Char], [Set Char])] -> String
part1 = ("Part 1: " ++) . show . length . filter ((`elem` lengths) . length) . concatMap snd
  where
    lengths = length . (digits !!) <$> [1, 4, 7, 8]
part2 = ("Part 2: " ++) . show . sum . map (uncurry decode)

decode :: [Set Char] -> [Set Char] -> Int
decode = wireOutput . ap zip connectWires
  where
    wireOutput r = concatDigits . mapMaybe (`lookup` r)
    connectWires = mapMaybe (flip elemIndex $ wires digits) . wires

wires :: Ord a => [Set a] -> [(Int, Int)]
wires signals = map (\d -> (count S.isSubsetOf d, count (flip S.isSubsetOf) d)) signals
  where
    count f d = length $ filter (f d) signals

--   0:      1:      2:      3:      4:
--  aaaa    ....    aaaa    aaaa    ....
-- b    c  .    c  .    c  .    c  b    c
-- b    c  .    c  .    c  .    c  b    c
--  ....    ....    dddd    dddd    dddd
-- e    f  .    f  e    .  .    f  .    f
-- e    f  .    f  e    .  .    f  .    f
--  gggg    ....    gggg    gggg    ....

--   5:      6:      7:      8:      9:
--  aaaa    aaaa    aaaa    aaaa    aaaa
-- b    .  b    .  .    c  b    c  b    c
-- b    .  b    .  .    c  b    c  b    c
--  dddd    dddd    ....    dddd    dddd
-- .    f  e    f  .    f  e    f  .    f
-- .    f  e    f  .    f  e    f  .    f
--  gggg    gggg    ....    gggg    gggg
digits :: [Set Char]
digits = map S.fromAscList ["abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg"]

parse :: String -> ([Set Char], [Set Char])
parse = (\[a, b] -> (a, b)) <$> map (map S.fromList . words) . splitOn " | "

concatDigits :: [Int] -> Int
concatDigits = foldl' ((+) . (* 10)) 0