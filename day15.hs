{-# LANGUAGE TupleSections #-}

import Data.Char (digitToInt)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Data.Heap (Heap)
import qualified Data.Heap as H
import Data.Tuple (swap)
import Control.Monad (ap)

type Point = (Int, Int)

type Grid a = Map Point a

main = interact (unlines . sequence [part1, part2] . grid . lines)

part1, part2 :: Grid Int -> String
part1 = ("Part 1: " ++) . safestPathLength
part2 = ("Part 2: " ++) . safestPathLength . enlarge 5

safestPathLength = maybe "Path not found" show . ap (`dijkstra` (0, 0)) (fst . M.findMax)

dijkstra :: Grid Int -> Point -> Point -> Maybe Int
dijkstra grid start end = dijkstra' initialHeap initialDistances
  where
    initialHeap :: Heap (Int, Point)
    initialHeap = H.singleton (0, start)
    initialDistances = M.insert start 0 (M.map (const (maxBound :: Int)) grid)
    dijkstra' heap distances = do
      ((dist, point), heap') <- H.uncons heap
      if point == end
        then return dist
        else
          let safer = M.differenceWith pickSafer (neighbors point distances) grid
              pickSafer d risk = if d > dist + risk then Just (dist + risk) else Nothing
           in dijkstra' (toHeap safer <> heap') (safer <> distances)


rollingRisk :: Int -> Int
rollingRisk risk = (risk - 1) `mod` 9 + 1

enlarge :: Int -> Grid Int -> Grid Int
enlarge n grid = M.fromList [newPoint ix iy x y | x <- [0..maxX], y <- [0..maxY], ix <- [0..n-1], iy <- [0..n-1]]
  where
    newPoint ix iy x y = (((maxX + 1) * ix + x, (maxY + 1) * iy + y), rollingRisk (ix + iy + grid M.! (x, y)))
    (maxX, maxY) = fst $ M.findMax grid


toHeap :: Grid Int -> Heap (Int, Point)
toHeap = H.fromList . map swap .  M.toList

grid :: [String] -> Grid Int
grid rows = M.fromList [((x, y), digitToInt col) | (cols, y) <- zip rows [0 ..], (col, x) <- zip cols [0 ..]]

neighbors :: Point -> Grid a -> Grid a
neighbors point grid = M.fromList $ mapMaybe (\a -> (a,) <$> M.lookup a grid) $ neighborPoints point

neighborPoints :: (Num a, Num b, Enum a, Enum b, Eq a, Eq b) => (a, b) -> [(a, b)]
neighborPoints (x, y) = [(x + dx, y + dy) | (dx, dy) <- [(1, 0), (0, 1), (-1, 0), (0, -1)]]
