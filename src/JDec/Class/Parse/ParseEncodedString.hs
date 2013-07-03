module JDec.Class.Parse.ParseEncodedString (
parseString
) where

import Data.Text as T(Text)
import Data.Text.Encoding as E(decodeUtf32LE)
import Data.ByteString.Lazy as BS (ByteString, toStrict, unpack)
import Data.ByteString.Lazy.Builder as BSB (toLazyByteString, word32LE)
import Data.Monoid ((<>),mempty)
import Data.Word(Word8, Word32)
import Data.Bits (shiftL, (.&.))

-- | Character accumulator for string decoding
data CharAccum = EmptyAccum -- ^ Empty
  | OneByteOfTwoAccum Word8 -- ^ One byte out of two
  | OneByteOfMoreThanTwoAccum Word8 -- ^ One byte out of more than two
  | TwoBytesAccum Word8 Word8 -- ^ Two bytes
  | ThreeBytesAccum Word8 Word8 Word8 -- ^ Three bytes
  | FourBytesAccum Word8 Word8 Word8 Word8 -- ^ Four bytes
  | FiveBytesAccum Word8 Word8 Word8 Word8 Word8 -- ^ Five bytes

-- | Convert UTF-32LE string into Text
parseString :: ByteString -- ^ UTF-32LE String
  -> Text -- ^ Text
parseString bytes = decodeUtf32LE (toStrict (deserializeString bytes))

-- | Convert modified UTF-8 to UTF-32LE
deserializeString :: ByteString -- ^ Modified UTF-8 string
  -> ByteString -- ^ UTF-32LE String
deserializeString bytes = toLazyByteString (handleTail (foldl handleNext (mempty,EmptyAccum) (unpack bytes)))
  where
    handleNext (bldr, accum) byte
      | (byte == 0x00) = (bldr, EmptyAccum)
      | ((byte >= 0xf0) && (byte <= 0xff)) = (bldr, EmptyAccum)
      | otherwise = 
        case accum of
          EmptyAccum -> handleNextChar bldr byte
          OneByteOfTwoAccum a -> handleOneByteOfTwoAccum bldr a byte
          OneByteOfMoreThanTwoAccum a -> (bldr, TwoBytesAccum a byte)
          TwoBytesAccum a b -> handleTwoBytesAccum bldr a b byte
          ThreeBytesAccum a b c -> handleThreeBytesAccum bldr a b c byte
          FourBytesAccum a b c d -> (bldr, FiveBytesAccum a b c d byte)
          FiveBytesAccum a b c d e -> handleFiveBytesAccum bldr a b c d e byte
    handleNextChar bldr byte
    -- A one byte char
      | isOneByte byte = ((bldr <> (BSB.word32LE (fromIntegral byte))), EmptyAccum)
    -- The first byte of a two byte char
      | isFirstByteOfTwoBytes byte = (bldr, OneByteOfTwoAccum byte)
    -- The first byte of a three byte char or a six byte char
      | isFirstByteOfThreeOrSixBytes byte = (bldr, OneByteOfMoreThanTwoAccum byte)
    -- Skip an invalid char
      | otherwise = (bldr, EmptyAccum)
    handleOneByteOfTwoAccum bldr accumByteOne byte
    -- The second byte of a two byte char
      | isSecondOfTwoBytes byte = (bldr <> (BSB.word32LE (buildTwoBytesChar accumByteOne byte)), EmptyAccum)
    -- Skip an invalid char
      | otherwise = (bldr, EmptyAccum)
    handleTwoBytesAccum bldr accumByteOne accumByteTwo byte
    -- May be three high bytes of a six byte char
      | ((accumByteOne == 0xed) && (accumByteTwo >= 0xa0) && (accumByteTwo <= 0xaf) && (byte >= 0x80) && (byte <= 0xbf)) = (bldr, ThreeBytesAccum accumByteOne accumByteTwo byte)
    -- The third byte of a three byte char
      | ((accumByteTwo >= 0x80) && (accumByteTwo <= 0xbf) && (byte >= 0x80) && (byte <= 0xbf)) = (bldr <> (BSB.word32LE (buildThreeBytesChar accumByteOne accumByteTwo byte)), EmptyAccum)
    -- Skip an invalid char
      | otherwise = (bldr, EmptyAccum)
    handleThreeBytesAccum bldr accumByteOne accumByteTwo accumByteThree byte
    -- May be four high bytes of a six byte char
      | (byte == 0xed) = (bldr, FourBytesAccum accumByteOne accumByteTwo accumByteThree byte)
    -- A three byte char and a one byte char
      | isOneByte byte = (((bldr <> (BSB.word32LE (buildThreeBytesChar accumByteOne accumByteTwo accumByteThree))) <> (BSB.word32LE (fromIntegral byte))), EmptyAccum)
    -- A three byte char and the first byte of a two byte char
      | isFirstByteOfTwoBytes byte = (bldr <> (BSB.word32LE (buildThreeBytesChar accumByteOne accumByteTwo accumByteThree)), OneByteOfTwoAccum byte)
    -- A three byte char and the first byte of a three byte char or a six byte char
      | isFirstByteOfThreeOrSixBytes byte = (bldr <> (BSB.word32LE (buildThreeBytesChar accumByteOne accumByteTwo accumByteThree)), OneByteOfMoreThanTwoAccum byte)
    -- A three byte char and an invalid char
      | otherwise = (bldr <> (BSB.word32LE (buildThreeBytesChar accumByteOne accumByteTwo accumByteThree)), EmptyAccum)
    handleFiveBytesAccum bldr accumByteOne accumByteTwo accumByteThree accumByteFour accumByteFive byte
    -- A six byte char
      | ((accumByteFour == 0xed) && (accumByteFive >= 0xb0) && (accumByteFive <= 0xbf) && (byte >= 0x80) && (byte <= 0xbf)) = (bldr <> (BSB.word32LE ((0x10000 :: Word32) + (shiftL ((fromIntegral accumByteTwo) .&. (0x0f :: Word32)) 16) + (shiftL ((fromIntegral accumByteThree) .&. (0x3f :: Word32)) 10) + (shiftL ((fromIntegral accumByteFive) .&. (0x0f :: Word32)) 6) + ((fromIntegral byte) .&. (0x3f :: Word32)))), EmptyAccum)
    -- A three byte char and maybe three high bytes of a six byte char
      | ((accumByteFive >= 0xa0) && (accumByteFive <= 0xaf) && (byte >= 0x80) && (byte <= 0xbf)) = (bldr <> (BSB.word32LE (buildThreeBytesChar accumByteOne accumByteTwo accumByteThree)), ThreeBytesAccum accumByteOne accumByteTwo accumByteThree)
    -- Two three byte chars
      | ((accumByteFive >= 0x80) && (accumByteFive <= 0xbf) && (byte >= 0x80) && (byte <= 0xbf)) = (((bldr <> (BSB.word32LE (buildThreeBytesChar accumByteOne accumByteTwo accumByteThree))) <> (BSB.word32LE (buildThreeBytesChar accumByteFour accumByteFive byte))), EmptyAccum)
    -- A three byte char and an invalid char
      | otherwise = (bldr <> (BSB.word32LE (buildThreeBytesChar accumByteOne accumByteTwo accumByteThree)), EmptyAccum)
    isOneByte byte = (byte >= 0x01) && (byte <= 0x7f)
    isFirstByteOfTwoBytes byte = (byte >= 0xc0) && (byte <= 0xdf)
    isFirstByteOfThreeOrSixBytes byte = (byte >= 0xe0) && (byte <= 0xef)
    isSecondOfTwoBytes byte = (byte >= 0x80) && (byte <= 0xbf)
    buildTwoBytesChar a b = ((shiftL ((fromIntegral a) .&. (0x1f :: Word32)) 6) + ((fromIntegral b) .&. (0x3f :: Word32)))
    buildThreeBytesChar a b c = (shiftL ((fromIntegral a) .&. (0x0f :: Word32)) 12) + (shiftL ((fromIntegral b) .&. (0x3f :: Word32)) 6) + ((fromIntegral c) .&. (0x3f :: Word32))
    handleTail (bldr,accum) =
      case accum of
        EmptyAccum -> bldr
        OneByteOfTwoAccum _ -> bldr
        OneByteOfMoreThanTwoAccum _ -> bldr
        TwoBytesAccum _ _ -> bldr
        ThreeBytesAccum a b c -> bldr <> (BSB.word32LE (buildThreeBytesChar a b c))
        FourBytesAccum a b c _ -> bldr <> (BSB.word32LE (buildThreeBytesChar a b c))
        FiveBytesAccum a b c _ _ -> bldr <> (BSB.word32LE (buildThreeBytesChar a b c))
