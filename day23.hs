module Main where

import Data.List
import Data.Char
import Debug.Trace
import Control.Monad (ap)
import qualified Data.IntMap as M

-- type BurrowMap = State ([String], [String])

main = interact (unlines . sequence [part1] . parse)

part1 = ("Part 1: "++) . show

parse = ap (,) createMap . lines

createMap layout = M.fromList [((x, y), cell) | (y, row) <- zip [0..] layout, (x, cell) <- zip [0..] row, isAlpha cell]