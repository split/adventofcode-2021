module Main where

import Control.Monad (ap)
import Data.Bool (bool)
import Data.Foldable (Foldable (foldl'))
import Data.Set (Set)
import qualified Data.Set as S

type Pixel = (Int, Int)

type Image = Set Pixel

main = interact (unlines . sequence [part1, part2] . enhance . parse)

part1, part2 :: [Image] -> String
part1 = ("Part 1: " ++) . show . S.size . (!! 2)
part2 = ("Part 2: " ++) . show . S.size . (!! 50)

enhance :: (Image, [Bool]) -> [Image]
enhance (image, algo) = scanl (flip (enhancementStep algo)) image spaces
  where
    spaces = if head algo then ap zip tail rotatingSpaces else repeat (False, False)
    rotatingSpaces = cycle [head algo, algo !! 511]

enhancementStep :: [Bool] -> (Bool, Bool) -> Image -> Image
enhancementStep algo (space, prevSpace) image = S.filter (\px -> prevSpace /= (algo !! mask px space image)) affected
  where
    affected = S.unions (S.map (S.fromList . win3x3) image)

-- >>> mask (2, 2) False $ image ["#..#.", "#....", "##..#", "..#..", "..###"]
-- 34
mask :: Pixel -> Bool -> Image -> Int
mask px space image = toDec $ map (\px' -> if px' `S.member` image then not space else space) (win3x3 px)

parse = ap ((,) . image . tail) (map (== '#') . head) . lines

image :: [String] -> Image
image rows = S.fromList [(x, y) | (cols, y) <- zip rows [0 ..], (pixel, x) <- zip cols [0 ..], pixel == '#']

win3x3 :: Pixel -> [Pixel]
win3x3 (x, y) = [(x', y') | y' <- [y -1 .. y + 1], x' <- [x -1 .. x + 1]]

-- >>> toDec $ map (== '#') "...#...#."
-- 34
toDec :: [Bool] -> Int
toDec = foldl' (\acc x -> acc * 2 + bool 0 1 x) 0