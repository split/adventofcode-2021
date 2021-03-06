{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad (msum)
import Data.Either (fromRight)
import Data.List (tails)
import Text.Parsec
import Text.Parsec.String (Parser)

infixr 5 :^:

data SnailNum a = Val a | (SnailNum a) :^: (SnailNum a) deriving (Eq, Ord)

main :: IO ()
main = interact (unlines . sequence [part1, part2] . map parseSN . lines)

part1, part2 :: [SnailNum Int] -> String
part1 = ("Part 1: " ++) . show . magnitude . foldl1 ((reduceSN .) . (:^:))
part2 = ("Part 2: " ++) . show . maximum . map (magnitude . reduceSN) . pairs

magnitude :: Integral a => SnailNum a -> a
magnitude (Val a) = a
magnitude (a :^: b) = 3 * magnitude a + 2 * magnitude b

reduceSN :: SnailNum Int -> SnailNum Int
reduceSN sn = case explodeSN sn of
  Just exploded -> reduceSN exploded
  Nothing -> maybe sn reduceSN (splitSN sn)

-- >>> explodeSN $ parseSN "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]"
-- Just [[[[0,7],4],[7,[[8,4],9]]],[1,1]]
-- >>> explodeSN $ parseSN "[[[[0,7],4],[7,[[8,4],9]]],[1,1]]"
-- Just [[[[0,7],4],[[7,8],[0,13]]],[1,1]]
explodeSN :: SnailNum Int -> Maybe (SnailNum Int)
explodeSN = (fst <$>) . go 0
  where
    go 4 (Val lv :^: Val rv) = Just (Val 0, (Just lv, Just rv))
    go 4 _ = Nothing
    go _ (Val _) = Nothing
    go d (l :^: r) = case (go (d + 1) l, go (d + 1) r) of
      (Just (l', s@(lv, Just rv)), _) -> case addRight rv r of
        Just r' -> Just (l' :^: r', (lv, Nothing))
        Nothing -> Just (l' :^: r, s)
      (Just (l', s), _) -> Just (l' :^: r, s)
      (_, Just (r', s@(Just lv, rv))) -> case addLeft lv l of
        Just l' -> Just (l' :^: r', (Nothing, rv))
        Nothing -> Just (l :^: r', s)
      (_, Just (r', s)) -> Just (l :^: r', s)
      _ -> Nothing

-- >>> addRight 3 $ parseSN "[[3,8],[1,1]]"
-- Just [[6,8],[1,1]]
-- >>> addLeft 3 $ parseSN "[[3,8],[1,1]]"
-- Just [[3,8],[1,4]]
addRight, addLeft :: Int -> SnailNum Int -> Maybe (SnailNum Int)
addLeft vl = alterR (\(Val v) -> Just (Val (v + vl)))
addRight vr = alterL (\(Val v) -> Just (Val (v + vr)))

-- >>> splitSN (Val 12)
-- Just [6,6]
splitSN :: Integral a => SnailNum a -> Maybe (SnailNum a)
splitSN = alterL splitRule
  where
    splitRule (Val n)
      | n > 9 = Just (Val (n `div` 2) :^: Val ((n + 1) `div` 2))
      | otherwise = Nothing

-- >>> parseSN "[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]"
-- [[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
parseSN :: String -> SnailNum Int
parseSN = fromRight (Val 0) . parse snailNumParser ""

snailNumParser :: Parser (SnailNum Int)
snailNumParser = chainl1 term op
  where
    op = (:^:) <$ char ','
    term = (Val . read <$> many1 digit) <|> brackets snailNumParser
    brackets = between (char '[') (char ']')

instance (Show a) => Show (SnailNum a) where
  show (Val value) = show value
  show (a :^: b) = "[" ++ show a ++ "," ++ show b ++ "]"

pairs :: [SnailNum a] -> [SnailNum a]
pairs l = concat [[x :^: y, y :^: x] | (x : ys) <- tails l, y <- ys]

alterL f (l :^: r) = msum [(:^: r) <$> alterL f l, (l :^:) <$> alterL f r]
alterL f sn@(Val _) = f sn

alterR f (l :^: r) = msum [(l :^:) <$> alterR f r, (:^: r) <$> alterR f l]
alterR f sn@(Val _) = f sn