{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (ap)
import Data.Bool (bool)
import Data.List (tails, unfoldr)
import Data.Maybe (listToMaybe, mapMaybe)

data Cuboid = Cuboid {x1 :: Int, x2 :: Int, y1 :: Int, y2 :: Int, z1 :: Int, z2 :: Int, on :: Bool}
  deriving (Show, Eq, Ord)

main :: IO ()
main = interact (unlines . sequence [part1, part2] . map parseCuboid . lines)

part1, part2 :: [Cuboid] -> String
part1 = ("Part 1: " ++) . show . sum . map volume . reboot . mapMaybe in50
part2 = ("Part 2: " ++) . show . sum . map volume . reboot

-- >>> in50 (Cuboid 20 60 20 60 20 60 False)
-- Just (Cuboid {x1 = 20, x2 = 50, y1 = 20, y2 = 50, z1 = 20, z2 = 50, on = False})
in50 :: Cuboid -> Maybe Cuboid
in50 = intersection (Cuboid (-50) 50 (-50) 50 (-50) 50 True)

reboot :: [Cuboid] -> [Cuboid]
reboot = foldl rebootStep []

rebootStep :: [Cuboid] -> Cuboid -> [Cuboid]
rebootStep cuboids cuboid =
  let intersecting = mapMaybe ((invert <$>) . intersection cuboid) cuboids
      cuboids' = cuboids ++ intersecting
   in if on cuboid then cuboid : cuboids' else cuboids'

intersection :: Cuboid -> Cuboid -> Maybe Cuboid
intersection a b
  | a `intersects` b = Just (Cuboid (max' x1) (min' x2) (max' y1) (min' y2) (max' z1) (min' z2) (on b))
  | otherwise = Nothing
  where
    min' f = f a `min` f b
    max' f = f a `max` f b

-- >>> Cuboid 0 1 0 1 0 1 `intersects` Cuboid 2 3 2 3 2 3
-- False
-- >>> Cuboid 0 1 0 1 0 1 `intersects` Cuboid 0 3 0 3 0 3
-- True
intersects :: Cuboid -> Cuboid -> Bool
a `intersects` b = x2 a >= x1 b && x2 b >= x1 a && y2 a >= y1 b && y2 b >= y1 a && z2 a >= z1 b && z2 b >= z1 a

volume :: Cuboid -> Int
volume Cuboid {..} = (x2 - x1 + 1) * (y2 - y1 + 1) * (z2 - z1 + 1) * bool (-1) 1 on

invert :: Cuboid -> Cuboid
invert cuboid = cuboid {on = not (on cuboid)}

parseCuboid :: [Char] -> Cuboid
parseCuboid = ap (toCuboid . unfoldr (listToMaybe . concatMap reads . tails)) isOn
  where
    toCuboid (x1 : x2 : y1 : y2 : z1 : z2 : _) = Cuboid x1 x2 y1 y2 z1 z2
    isOn ('o' : 'n' : _) = True
    isOn _ = False
