import Data.Char (isLower, isUpper)
import Data.List (foldl')
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

type Caves = Map String (Set String)

main = interact (unlines . sequence [part1, part2] . parseCaves)

part1, part2 :: Caves -> String
part1 = ("Part 1: " ++) . show . length . exploreCaves False
part2 = ("Part 2: " ++) . show . length . exploreCaves True

exploreCaves :: Bool -> Caves -> [[String]]
exploreCaves twice caves = go [["start"]]
  where
    go [] = []
    go (path@(cave : _) : paths)
      | cave == "end" = path : go paths
      | otherwise = go newPaths
      where
        possibleCaves = maybe S.empty (S.filter (\c -> c `notElem` path || all isUpper c || canVisitTwice c)) (M.lookup cave caves)
        newPaths = map (: path) (S.toList possibleCaves) ++ paths
        canVisitTwice cave = twice && cave /= "start" && hasNotVisitedTwice (init (tail path))

hasNotVisitedTwice :: [String] -> Bool
hasNotVisitedTwice [] = True
hasNotVisitedTwice (x : xs)
  | isLower (head x) && x `elem` xs = False
  | otherwise = hasNotVisitedTwice xs

parseCaves :: String -> Caves
parseCaves = foldl' parseCave M.empty . map (splitOn "-") . lines
  where
    addCave s e = M.insertWith S.union s (S.singleton e)
    parseCave m (s : e : _) = addCave s e (addCave e s m)
