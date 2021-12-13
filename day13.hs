import Data.Bifunctor (Bifunctor (first, second))
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as S

data Fold = FoldLeft Int | FoldTop Int deriving (Show)

type Coord = (Int, Int)

type Paper = Set Coord

main = interact (unlines . sequence [part1, part2] . uncurry paperFolds . parse)

part1, part2 :: [Paper] -> String
part1 = ("Part 1: " ++) . show . S.size . (!! 1)
part2 = intercalate "\n" . map ("Part 2: " ++) . showPaper . last

paperFolds :: Paper -> [Fold] -> [Paper]
paperFolds = scanl (flip foldPaper)

foldPaper :: Fold -> Paper -> Paper
foldPaper (FoldLeft a) = S.map (first $ foldSide a)
foldPaper (FoldTop a) = S.map (second $ foldSide a)

foldSide a xy = if a >= xy then xy else abs (xy - a * 2)

parse :: String -> (Paper, [Fold])
parse = parseChunks . map lines . splitOn "\n\n"
  where
    parseChunks (dots : folds : _) = (S.fromList $ map parseDot dots, map parseFold folds)
    parseFold = foldToCoord . splitOn "=" . last . words
    parseDot = dotToCoord . splitOn ","
    foldToCoord ("x" : x : _) = FoldLeft (read x)
    foldToCoord ("y" : y : _) = FoldTop (read y)
    foldToCoord _ = error "Invalid data"
    dotToCoord (x : y : _) = (read x, read y)
    dotToCoord _ = error "Invalid data"

showPaper :: Paper -> [String]
showPaper p = [showLine y | y <- [0 .. maxY]]
  where
    showLine y = [maybe '.' (const '#') (S.lookupIndex (x, y) p) | x <- [0 .. maxX]]
    maxX = maximum $ map fst $ S.toList p
    maxY = maximum $ map snd $ S.toList p