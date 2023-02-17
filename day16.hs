{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import Control.Arrow ((&&&))
import Control.Monad (replicateM, unless, when)
import Control.Monad.State
import qualified Data.Binary as BN
import Data.Binary.Bits.Get
import qualified Data.Binary.Bits.Get as BZ (Block (..))
import qualified Data.Binary.Get as BN
import qualified Data.Binary.Get as BNG
import Data.Binary.Parser.Numeric
import Data.Bits
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 (fromStrict, pack)
import Data.Int
import Data.Word
import Debug.Trace (trace)
import Numeric (readHex)
import Text.Printf (PrintfArg, printf)

data Packet
  = Literal {header :: Header, value :: Word16, size :: Int}
  | Operator {header :: Header, subPackets :: [Packet], size :: Int}
  deriving (Show)

data LengthType = TotalLength Int | Count Int deriving (Show)

getLengthType False = (TotalLength, 15)
getLengthType True = (Count, 11)

data Header = Header
  { version :: !Word8,
    typeID :: !Word8
  }
  deriving (Show)

versionSum :: Packet -> Word8
versionSum Literal {header} = version header
versionSum Operator {header, subPackets} = version header + sum (map versionSum subPackets)

main = interact (unlines . sequence [const demo])

data GetS = GetS !B.ByteString !Int !Int deriving (Show)

-- trace (printf "%b" (runGet hexadecimal "D2FE28" :: Integer))

-- "D2FE28" literals
-- "38006F45291200" two sub-packets
-- "EE00D40C823060" three sub-packets

demo = show $ BN.runGet (runBitGet parsePacket) $ (\p -> trace (show p) p) $ B.dropWhile (== 0) $ (\p -> trace (show p) p) $ BN.encode $ parseHex "8A004A801A8002F478"

parsePacket :: BitGet Packet
parsePacket = do
  header <- parseHeader
  res <- case typeID header of
    4 -> parseLiteral header
    _ -> parseOperator header
  trace (show res) return $! res

parseHeader :: BitGet Header
parseHeader = do
  !version <- getWord8 3
  !typeID <- getWord8 3
  trace (printf "%03b %d %03b %d" version version typeID typeID) return $! Header version typeID

parseLiteral :: Header -> BitGet Packet
parseLiteral header = ap (Literal header . concatBits 4) ((6 +) . (5 *) . length) <$> literalChunks
  where
    literalChunks = do
      !continue <- getBool
      !literal <- getWord16be 4
      if continue
        then (literal :) <$> literalChunks
        else return [literal]

parseOperator :: Header -> BitGet Packet
parseOperator header = do
  (lengthType, lengthSize) <- getLengthType <$> getBool
  length <- fromIntegral <$> getWord16be lengthSize
  subPackets <- parseSubPackets (lengthType length)
  return $! Operator header subPackets (6 + 1 + lengthSize)

-- parseSubPackets :: BitGet [Packet]
-- parseSubPackets = do
--   (lengthType, lengthSize) <- getLengthType <$> getBool
--   length <- fromIntegral <$> getWord16be lengthSize
--   trace ("Parse sub packets " ++ show (lengthType length)) parseSubPackets (lengthType length)

parseSubPackets :: LengthType -> BitGet [Packet]
parseSubPackets (Count n) = replicateM n parsePacket
parseSubPackets (TotalLength n) = do
  packet <- parsePacket
  let s = packetSize packet
  if s < n
    then (packet :) <$> parseSubPackets (TotalLength (n - s))
    else return []

-- !chunk <- B.drop ((32 - fromIntegral n) `div` 8) . BN.encode <$> getWord8 n
-- !chunk <- getLazyByteString 32
-- trace ("Reading chunk " ++ show chunk) return (BN.runGet (runBitGet parsePackets) chunk)

-- parseSubPackets (TotalLength n) = go 1
--   where
--    go i = do
--     packet <- parsePacket
--     if n > i then (packet :) <$> go (i + 1)
--     else return [packet]

packetSize :: Packet -> Int
packetSize Literal {size} = size
packetSize Operator {size, subPackets} = size + sum (map packetSize subPackets)

db :: PrintfArg a => a -> a
db x = trace (printf "%012b (%d)" x x) x

parseHex :: B.ByteString -> Int
parseHex = BN.runGet hexadecimal

concatBits :: (Bits a, Num a) => Int -> [a] -> a
concatBits n = go 0 . reverse
  where
    go _ [] = 0
    go 0 (b : xs) = b .|. go n xs
    go i (b : xs) = b `shiftL` i .|. go (i + n) xs