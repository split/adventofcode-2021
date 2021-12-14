import Control.Monad (ap)
import Data.Foldable (Foldable (foldr'))
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

type PolyPair = (Char, Char)

type PolyCount = Map Char Int

type PolymerMap = Map PolyPair Char

main :: IO ()
main = interact (unlines . sequence [part1, part2] . parse)

part1, part2 :: (String, PolymerMap) -> String
part1 = ("Part 1: " ++) . show . diffCommon . uncurry (polyCounts 10)
part2 = ("Part 2: " ++) . show . diffCommon . uncurry (polyCounts 40)

diffCommon :: PolyCount -> Int
diffCommon = ap ((-) . maximum) minimum . M.elems

polyCounts :: Int -> String -> Map PolyPair Char -> PolyCount
polyCounts n template rules = mergeCounts $ counts template : map (polyCount n) (polyPairs template)
  where
    polyCounts' n' = map (\n'' -> M.mapWithKey (const . polyCount n'') rules) [0 ..] !! n'
    polyCount :: Int -> PolyPair -> PolyCount
    polyCount 0 _ = M.empty
    polyCount n' p@(a, b) = case p `M.lookup` rules of
      Nothing -> M.empty
      Just c ->
        mergeCounts
          [ M.singleton c 1,
            (a, c) `polyLookup` polyCounts' (n' - 1),
            (c, b) `polyLookup` polyCounts' (n' - 1)
          ]

mergeCounts :: [PolyCount] -> PolyCount
mergeCounts = M.unionsWith (+)

counts :: String -> PolyCount
counts = foldr' (flip (M.insertWith (+)) 1) M.empty

parse :: String -> (String, PolymerMap)
parse = parseChunks . splitOn "\n\n"
  where
    parseChunks (template : pairs : _) = (template, parsePolymerPairs pairs)
    parsePolymerPairs = foldr' (parsePolymerPair . splitOn " -> ") M.empty . lines
    parsePolymerPair ((a : b : _) : (c : _) : _) = M.insert (a, b) c

polyPairs = ap zip tail

polyLookup = (fromMaybe M.empty .) . M.lookup