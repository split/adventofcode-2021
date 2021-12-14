import Control.Monad (ap)
import Data.Foldable (Foldable (foldr'))
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M

type PolyPair = (Char, Char)

type PolymerCount = Map PolyPair Int

type MonomerCount = Map Char Int

type PolymerMap = Map PolyPair Char

main :: IO ()
main = interact (unlines . sequence [part1, part2] . uncurry polyCounts . parse)

part1, part2 :: [PolymerCount] -> String
part1 = ("Part 1: " ++) . show . diffCommon . monomerCount . (!! 10)
part2 = ("Part 2: " ++) . show . diffCommon . monomerCount . (!! 40)

diffCommon :: Map a Int -> Int
diffCommon = ap ((-) . maximum) minimum . M.elems

polyCounts :: PolymerMap -> [PolyPair] -> [PolymerCount]
polyCounts rules = polyCounts' . counts
  where
    polyCounts' pc = pc : polyCounts' (M.fromListWith (+) $ concatMap (uncurry polymerization) (M.toList pc))
    polymerization p@(a, b) count = let c = rules M.! p in [((a, c), count), ((c, b), count)]

monomerCount :: PolymerCount -> MonomerCount
monomerCount = M.mapKeysWith (+) snd

counts :: (Ord a) => [a] -> Map a Int
counts = foldr' (flip (M.insertWith (+)) 1) M.empty

parse :: String -> (PolymerMap, [PolyPair])
parse = parseChunks . splitOn "\n\n"
  where
    parseChunks (template : pairs : _) = (parsePolymerPairs pairs, polyPairs template)
    parsePolymerPairs = foldr' (parsePolymerPair . splitOn " -> ") M.empty . lines
    parsePolymerPair ((a : b : _) : (c : _) : _) = M.insert (a, b) c
    polyPairs = ap zip tail