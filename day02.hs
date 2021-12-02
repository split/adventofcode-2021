main :: IO ()
main = interact (unlines . sequence [part1, part2] . map (parse . words) . lines)

part1 :: [(Int, Int)] -> String
part1 = (++) "Part 1: " <$> show . uncurry (*) . foldl add (0, 0)

part2 = (++) "Part 2: " <$> show . uncurry (*) . fst . foldl foldAim ((0, 0), 0)
  where
    foldAim (p@(x2, y2), aim) (x, y)
      | x > 0 = ((x2 + x, y2 + aim * x), aim)
      | otherwise = (p, aim + y)

parse :: [String] -> (Int, Int)
parse ["forward", n] = (read n, 0)
parse ["down", n] = (0, read n)
parse ["up", n] = (0, - read n)

add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)