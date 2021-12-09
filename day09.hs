{-# LANGUAGE TupleSections #-}

import Control.Monad (ap)
import Data.Char (digitToInt)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.Ord (Down (Down))

type Coord = (Int, Int)

type Grid a = Map Coord a

main :: IO ()
main = interact (unlines . sequence [part1, part2] . grid . lines)

part1, part2 :: Grid Int -> String
part1 = ("Part 1: " ++) . show . sum . map (+ 1) . M.elems . lowPoints
part2 grid = ("Part 2: " ++) . show $ product $ take 3 . sortOn Down . map (M.size . getBasin grid) $ separate $ lowPoints grid

lowPoints :: Grid Int -> Grid Int
lowPoints grid = M.filterWithKey (\c h -> all (> h) (neighbors grid c)) grid

getBasin :: Grid Int -> Grid Int -> Grid Int
getBasin grid = growBasin M.empty
  where
    growBasin basin newBasin
      | M.null newBasin = basin
      | otherwise = ap growBasin (nextBasin newBasin) (M.union basin newBasin)
    nextBasin = (M.\\) . M.foldrWithKey ((flip M.union .) . slope) M.empty
      where
        slope coord value = M.filter (\v -> v > value && v < 9) (neighbors grid coord)

grid :: [String] -> Grid Int
grid rows = M.fromList [((x, y), digitToInt col) | (cols, y) <- zip rows [0 ..], (col, x) <- zip cols [0 ..]]

neighbors :: Grid a -> Coord -> Grid a
neighbors grid coord = M.fromList $ mapMaybe ((\c -> (c,) <$> M.lookup c grid) . add coord) [(0, 1), (1, 0), (0, -1), (-1, 0)]

separate :: Grid a -> [Grid a]
separate = M.foldrWithKey (\k v b -> b ++ [M.singleton k v]) []

add :: Coord -> Coord -> Coord
add (x, y) (x', y') = (x + x', y + y')