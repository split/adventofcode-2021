import Control.Arrow (Arrow ((&&&)))
import Control.Monad (liftM2)
import Data.Char (digitToInt)
import Data.Function (on)
import Data.List

main = interact (unlines . sequence [part1, part2] . lines)

part1 :: [String] -> String
part1 = ("Part 1: " ++) . show . product . map toDec . gammaAndEpsilon

gammaAndEpsilon :: [[Char]] -> [[Char]]
gammaAndEpsilon = transpose . map (map head . sortOn length . group . sort . (++) "01") . transpose

part2 :: [String] -> String
part2 = ("Part 2: " ++) . show . liftM2 (*) oxygen co2
  where
    oxygen = toDec . supportRating mostCommon
    co2 = toDec . supportRating leastCommon

supportRating :: (String -> Char) -> [String] -> String
supportRating criteria report = head $ go report 0
  where
    go [result] _ = [result]
    go pool i = go matching (i + 1)
      where
        matching = map fst $ filter (\x -> common == snd x) withBit
        withBit = map (\x -> (x, x !! i)) pool
        common = criteria $ map snd withBit

mostCommon :: String -> Char
mostCommon = fst . maximumBy (compare `on` snd) . count

leastCommon :: String -> Char
leastCommon = fst . minimumBy (compare `on` snd) . count

count :: String -> [(Char, Int)]
count = map (head &&& length) . group . sort

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0