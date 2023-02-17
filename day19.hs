{-# LANGUAGE TupleSections #-}

module Main where

import Control.Monad (ap, join)
import Data.Either
import Data.Foldable
import Data.List
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Debug.Trace

data Vec3 a = Vec3 a a a deriving (Eq, Ord, Show)

type Matrix a = Vec3 (Vec3 a)

type Grid a = Map (Vec3 a) [a]

type Scanner = Map (Vec3 Int) [Int]

data Dir = Y | X | Z | YNeg | XNeg | ZNeg deriving (Bounded, Enum, Eq, Show)

main :: IO ()
main = interact (unlines . sequence [part1] . parseInput)

part1 :: [Scanner] -> [Char]
part1 = show

-- part1 = ("Part 1: " ++) . show . M.size . M.unionsWith intersect . alignAll

alignAll :: [Scanner] -> [Scanner]
alignAll [] = []
alignAll (scanner0 : xs) = go xs [scanner0]
  where
    go :: [Scanner] -> [Scanner] -> [Scanner]
    go [] aligned = aligned
    go xs aligned =
      let (xs', newAligned) = partitionEithers $ map (align (M.unions aligned)) xs
       in go (newAligned ++ aligned) xs'

-- let (m, xs') = partition (isJust . snd) $ map (ap (,) (align (M.unions aligned))) xs
--     aligned' = aligned ++ mapMaybe snd m
--  in trace (show $ mapMaybe snd m) go aligned' (map fst xs')

-- |
-- >>> let a = M.fromList [((0, 1, 5), [2, 6, 8, 10, 12, 40])] :: Grid [Int]
-- >>> let b = M.fromList [((0, 3, 2), [1, 6, 14, 10, 12, 40])] :: Grid [Int]
-- >>> match a b
-- Just (fromList [((0,1,5),True)])
align :: Scanner -> Scanner -> Either Scanner Scanner
align a b = if M.size matching >= 12 then align' matching a b else Left b
  where
    matching = getMatchingBeacons a b
    alignPositions =
      let transform = trace (show matching) find (\t -> any (\(ak, bk) -> t (bk - ak) == bk) (M.toList matching)) transforms -- trace (show (bk, ak, bk - ak))
       in case transform of
            Just t -> trace ("FOUND SOMETHING" ++ show (t (Vec3 1 1 1))) Right (M.mapKeys t b)
            Nothing -> Left b

align' :: Map (Vec3 Int) (Vec3 Int) -> Scanner -> Scanner -> Either Scanner Scanner
align' matching ax bx = case find (\t -> any (findT t) (M.toList matching)) transforms of
  Just transform -> Right (M.mapKeys transform bx)
  Nothing -> Left bx
  where
    findT transform (a, b) = any (\a' -> transform (b - a') == a) (M.keys ax)

-- align' :: Scanner -> Scanner -> Either Scanner Scanner
-- align' ax bx = case find ((>= 12) . length . take 12 . snd) $ map findT transforms of
--   Just (transform, _) -> trace ("FOUND SOMETHING" ++ show (transform (Vec3 1 1 1))) Right (M.mapKeys transform bx)
--   Nothing -> trace ("NO LUCK") Left bx
--   where
--     ap = M.keys ax
--     bp = M.keys bx
--     findT transform = (transform, catMaybes [(a' + b - a) `M.lookup` ax | a' <- ap, a <- ap, b <- map transform bp])

-- translationMatrix = uncurry getTranslationMatrix . M.elemAt 0

getMatchingBeacons :: Scanner -> Scanner -> Map (Vec3 Int) (Vec3 Int)
getMatchingBeacons a b = M.mapMaybe (\ad -> fst <$> find (\(_, bd) -> length (ad `intersect` bd) > 3) (M.toList b)) a

-- centroid :: Scanner -> Vec3 Int
-- centroid a = fmap (\v -> v `div` M.size a) . sum $ M.keys a

-- getTranslationMatrix :: Integral a => Vec3 a -> Vec3 a -> [[a]]
-- getTranslationMatrix (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) =
--   let dot = mmult [[x1, y1, z1]] [[x2, y2, z2]]
--    in dot

-- >>> length rm
-- 20
-- rm = ro : mneg ro : mx : mneg mx : my : mneg my : mz : mneg mz : rotations rx ++ rotations ry ++ rotations rz

-- rm = [mx | rot <- rotations rx]

-- >>> map (mmult [[3,1,2]]) [rx, ry, rz, mx, my, mz]
-- [[[3,2,-1]],[[-2,1,3]],[[1,-3,2]],[[-3,1,2]],[[3,-1,2]],[[3,1,-2]]]
rx, ry, rz, mx, my, mz :: [[Int]]
rx = [[1, 0, 0], [0, 0, -1], [0, 1, 0]]
ry = [[0, 0, 1], [0, 1, 0], [-1, 0, 0]]
rz = [[0, -1, 0], [1, 0, 0], [0, 0, 1]]
mx = [[-1, 0, 0], [0, 1, 0], [0, 0, 1]]
my = [[1, 0, 0], [0, -1, 0], [0, 0, 1]]
mz = [[1, 0, 0], [0, 1, 0], [0, 0, -1]]

-- >>> length $ transforms
-- 24
transforms :: Num a => [Vec3 a -> Vec3 a]
transforms = [rot . lookAt dir | dir <- [minBound .. maxBound], rot <- rotations]

lookAt :: Num a => Dir -> Vec3 a -> Vec3 a
lookAt dir (Vec3 x y z) = case dir of
  X -> Vec3 y x (- z)
  Y -> Vec3 x y z
  Z -> Vec3 y z x
  XNeg -> Vec3 y (- x) z
  YNeg -> Vec3 y (- y) (- z)
  ZNeg -> Vec3 y (- z) (- x)

rotations :: (Num a) => [Vec3 a -> Vec3 a]
rotations =
  [ \(Vec3 x y z) -> Vec3 x y z,
    \(Vec3 x y z) -> Vec3 z y (- x),
    \(Vec3 x y z) -> Vec3 (- x) y (- z),
    \(Vec3 x y z) -> Vec3 (- z) y x
  ]

-- >>> take 4 $ rotations rx
-- [[[ 1, 0, 0],[ 0, 0,-1],[ 0, 1, 0]],
--  [[ 1, 0, 0],[ 0,-1, 0],[ 0, 0,-1]],
--  [[ 1, 0, 0],[ 0, 0, 1],[ 0,-1, 0]],
--  [[ 1, 0, 0],[ 0, 1, 0],[ 0, 0, 1]]]
-- rotations rm' = take 4 $ iterate (mmult rm') rm'

parseInput = map (grid . tail . lines) . splitOn "\n\n"

grid :: [String] -> Grid Int
grid coordinates = M.fromList $ tri $ map parseCoorinate coordinates
  where
    tri coords = [(c, triangulate c coords) | c <- coords]
    parseCoorinate = toVec3 . map read . splitOn ","

mmult :: (Num a) => [[a]] -> [[a]] -> [[a]]
mmult a b = [[sum $ zipWith (*) ar bc | bc <- transpose b] | ar <- a]

triangulate :: (Ord a, Num a) => Vec3 a -> [Vec3 a] -> [a]
triangulate = (sort .) . map . distanceSq

-- >>> distanceSq (Vec3 1 (-1) 1) (Vec3 6 (-3) 3)
-- 33
-- >>> manhattan (Vec3 1 (-1) 1) (Vec3 6 (-3) 3)
-- 9
distanceSq, manhattan :: Num a => Vec3 a -> Vec3 a -> a
distanceSq v1 v2 = abs . vsum3 . join (*) $ (v2 - v1)
manhattan v1 v2 = vsum3 . fmap abs $ (v2 - v1)

toVec3 :: [a] -> Vec3 a
toVec3 (x : y : z : _) = Vec3 x y z
toVec3 _ = error "Invalid data"

vsum3 :: Num a => Vec3 a -> a
vsum3 (Vec3 x y z) = x + y + z

instance Functor Vec3 where
  fmap f (Vec3 x y z) = Vec3 (f x) (f y) (f z)

-- >>> Vec3 1 2 3 + Vec3 4 7 8
-- Vec3 5 9 11
instance (Num a) => Num (Vec3 a) where
  (+) = binVec3 (+)
  (-) = binVec3 (-)
  (*) = binVec3 (*)
  abs = fmap abs
  signum = fmap signum
  negate = fmap negate
  fromInteger v = let v' = fromInteger v in Vec3 v' v' v'

binVec3 :: (a -> a -> a) -> Vec3 a -> Vec3 a -> Vec3 a
binVec3 f (Vec3 x y z) (Vec3 x' y' z') = Vec3 (x `f` x') (y `f` y') (z `f` z')