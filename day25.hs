{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (ap)
import Data.Bifunctor
import Data.Map (Map)
import qualified Data.Map as M
import Debug.Trace

data SeaCucumber = SouthCucumber | EastCucumber deriving (Eq)

instance Show SeaCucumber where
  show SouthCucumber = "v"
  show EastCucumber = ">"

type Pos = (Int, Int)

type Sea = Map Pos SeaCucumber

data World = World {sea :: Sea, width :: Int, height :: Int} deriving (Show, Eq)

main = interact (unlines . sequence [part1] . parse . lines)

part1 = ("Part 1: " ++) . show . (+ 1) . length . takeWhile (uncurry (/=)) . ap zip tail . iterate step

step :: World -> World
step world@World {..} = world {sea = M.fromList $ map nextPos $ M.assocs sea}
  where
    nextEastPos (x, y) = ((x + 1) `mod` width, y)
    prevEastPos (x, y) = ((width + x - 1) `mod` width, y)
    nextSouthPos (x, y) = (x, (y + 1) `mod` height)
    nextPos current@(pos, EastCucumber) = maybe (first nextEastPos current) (const current) (sea M.!? nextEastPos pos)
    nextPos current@(pos, SouthCucumber) = case sea M.!? nextSouthPos pos of
      Just SouthCucumber -> current
      Just EastCucumber -> maybe (first nextSouthPos current) (const current) (sea M.!? nextSouthPos (nextEastPos pos))
      Nothing -> case sea M.!? nextSouthPos (prevEastPos pos) of
        Just EastCucumber -> current
        _ -> first nextSouthPos current

parse :: [String] -> World
parse waves = World (parseSea waves) (length (head waves)) (length waves)

parseSea :: [String] -> Sea
parseSea waves = M.fromList [((x, y), cucumber c) | (y, xs) <- zip [0 ..] waves, (x, c) <- zip [0 ..] xs, c /= '.']

cucumber :: Char -> SeaCucumber
cucumber '>' = EastCucumber
cucumber _ = SouthCucumber

printWorld :: World -> String
printWorld World {..} = unlines (map printWave [0 .. height - 1])
  where
    printWave y = concat [maybe "." show (sea M.!? (x, y)) | x <- [0 .. width - 1]]