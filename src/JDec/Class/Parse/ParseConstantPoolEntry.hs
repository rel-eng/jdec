module JDec.Class.Parse.ParseConstantPoolEntry (
deserializeConstantPoolEntry
) where 

import JDec.Class.Raw.ConstantPoolEntry (
  ConstantPoolEntry(
    UTF8ConstantPoolEntry,
    IntegerConstantPoolEntry,
    FloatConstantPoolEntry,
    LongConstantPoolEntry,
    DoubleConstantPoolEntry,
    ClassConstantPoolEntry,
    StringConstantPoolEntry,
    FieldRefConstantPoolEntry,
    MethodRefConstantPoolEntry,
    InterfaceMethodRefConstantPoolEntry,
    NameAndTypeConstantPoolEntry,
    MethodHandleConstantPoolEntry,
    MethodTypeConstantPoolEntry,
    InvokeDynamicConstantPoolEntry))
import JDec.Class.Raw.ConstantPoolIndex (ConstantPoolIndex(ConstantPoolIndex))
import JDec.Class.Raw.MethodHandleKind(
  MethodHandleKind(
    GetFieldMethodHandleKind,
    GetStaticMethodHandleKind,
    PutFieldMethodHandleKind,
    PutStaticMethodHandleKind,
    InvokeVirtualMethodHandleKind,
    InvokeStaticMethodHandleKind,
    InvokeSpecialMethodHandleKind,
    NewInvokeSpecialMethodHandleKind,
    InvokeInterfaceMethodHandleKind))

import Data.Binary.Get(Get,getWord8,getWord16be,getWord32be,getLazyByteString)
import Data.Text as T(Text)
import Data.Text.Encoding as E(decodeUtf32LE)
import Data.ByteString.Lazy as BS (ByteString, toStrict, unpack)
import Data.ByteString.Lazy.Builder as BSB (toLazyByteString, word32LE)
import Data.Monoid ((<>),mempty)
import Data.Word(Word8, Word32, Word64)
import Data.Int(Int32, Int64)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))

-- | Character accumulator for string decoding
data CharAccum = EmptyAccum -- ^ Empty
  | OneByteOfTwoAccum Word8 -- ^ One byte out of two
  | OneByteOfMoreThanTwoAccum Word8 -- ^ One byte out of more than two
  | TwoBytesAccum Word8 Word8 -- ^ Two bytes
  | ThreeBytesAccum Word8 Word8 Word8 -- ^ Three bytes
  | FourBytesAccum Word8 Word8 Word8 Word8 -- ^ Four bytes
  | FiveBytesAccum Word8 Word8 Word8 Word8 Word8 -- ^ Five bytes

-- | Deserialize one constant pool entry
deserializeConstantPoolEntry :: Get (ConstantPoolEntry) -- ^ Contant pool entry
deserializeConstantPoolEntry = do
  tag <- getWord8
  case tag of
    1 -> do
      len <- getWord16be
      bytes <- getLazyByteString (fromIntegral len)
      return $! UTF8ConstantPoolEntry (parseString bytes)
    3 -> do
      val <-getWord32be
      return $! IntegerConstantPoolEntry (toInteger ((fromIntegral val) :: Int32))
    4 -> do
      val <- getWord32be
      return $! FloatConstantPoolEntry (deserializeFloat val)
    5 -> do
      high <-getWord32be
      low <-getWord32be
      return $! LongConstantPoolEntry (toInteger ((fromIntegral ((shiftL ((fromIntegral high) :: Word64) 32) + ((fromIntegral low) :: Word64))) :: Int64))
    6 -> do
      high <-getWord32be
      low <-getWord32be
      return $! DoubleConstantPoolEntry (deserializeDouble ((shiftL ((fromIntegral high) :: Word64) 32) + ((fromIntegral low) :: Word64)))
    7 -> do
      idx <- getWord16be
      return $! ClassConstantPoolEntry (ConstantPoolIndex (toInteger idx))
    8 -> do
      idx <- getWord16be
      return $! StringConstantPoolEntry (ConstantPoolIndex (toInteger idx))
    9 -> do
      classIdx <- getWord16be
      nameAndTypeIdx <- getWord16be
      return $! FieldRefConstantPoolEntry (ConstantPoolIndex (toInteger classIdx)) (ConstantPoolIndex (toInteger nameAndTypeIdx))
    10 -> do
      classIdx <- getWord16be
      nameAndTypeIdx <- getWord16be
      return $! MethodRefConstantPoolEntry (ConstantPoolIndex (toInteger classIdx)) (ConstantPoolIndex (toInteger nameAndTypeIdx))
    11 -> do
      classIdx <- getWord16be
      nameAndTypeIdx <- getWord16be
      return $! InterfaceMethodRefConstantPoolEntry (ConstantPoolIndex (toInteger classIdx)) (ConstantPoolIndex (toInteger nameAndTypeIdx))
    12 -> do
      nameIdx <- getWord16be
      descriptorIdx <- getWord16be
      return $! NameAndTypeConstantPoolEntry (ConstantPoolIndex (toInteger nameIdx)) (ConstantPoolIndex (toInteger descriptorIdx))
    15 -> do
      refKind <- getWord8
      refIdx <- getWord16be
      case refKind of
        1 -> return $! MethodHandleConstantPoolEntry GetFieldMethodHandleKind (ConstantPoolIndex (toInteger refIdx))
        2 -> return $! MethodHandleConstantPoolEntry GetStaticMethodHandleKind (ConstantPoolIndex (toInteger refIdx))
        3 -> return $! MethodHandleConstantPoolEntry PutFieldMethodHandleKind (ConstantPoolIndex (toInteger refIdx))
        4 -> return $! MethodHandleConstantPoolEntry PutStaticMethodHandleKind (ConstantPoolIndex (toInteger refIdx))
        5 -> return $! MethodHandleConstantPoolEntry InvokeVirtualMethodHandleKind (ConstantPoolIndex (toInteger refIdx))
        6 -> return $! MethodHandleConstantPoolEntry InvokeStaticMethodHandleKind (ConstantPoolIndex (toInteger refIdx))
        7 -> return $! MethodHandleConstantPoolEntry InvokeSpecialMethodHandleKind (ConstantPoolIndex (toInteger refIdx))
        8 -> return $! MethodHandleConstantPoolEntry NewInvokeSpecialMethodHandleKind (ConstantPoolIndex (toInteger refIdx))
        9 -> return $! MethodHandleConstantPoolEntry InvokeInterfaceMethodHandleKind (ConstantPoolIndex (toInteger refIdx))
        _ -> fail ("Unknow method handle kind " ++ (show refKind))
    16 -> do
      descriptorIdx <- getWord16be
      return $! MethodTypeConstantPoolEntry (ConstantPoolIndex (toInteger descriptorIdx))
    18 -> do
      bootstrapMethodAttrIdx <- getWord16be
      nameAndTypeIdx <- getWord16be
      return $! InvokeDynamicConstantPoolEntry (toInteger bootstrapMethodAttrIdx) (ConstantPoolIndex (toInteger nameAndTypeIdx))
    _ -> fail ("Unknown constant pool entry type " ++ (show tag))

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

-- | Read float from 32 bit word
deserializeFloat :: Word32 -- ^ 32 bit word
  -> Float -- ^ Float
deserializeFloat val
  | (val == 0x7f800000) = read "Infinity"
  | (val == 0xff800000) = read "-Infinity"
  | ((val >= 0x7f800001) && (val <= 0x7fffffff)) = read "NaN"
  | ((val >= 0xff800001) && (val <= 0xffffffff)) = read "NaN"
  | otherwise = encodeFloat ((extractFloatSign val)*(extractFloatMantissa val)) (fromIntegral ((extractFloatExponent val) - 150))

-- | Read float sign from 32 bit word
extractFloatSign :: Word32 -- ^ 32 bit word
  -> Integer -- ^ 1 for positive, -1 for negative
extractFloatSign val = if ((shiftR val 31) == 0) then (1 :: Integer) else (-1 :: Integer)

-- | Read float exponent from 32 bit word
extractFloatExponent :: Word32 -- ^ 32 bit word
  -> Integer -- ^ float exponent + 150
extractFloatExponent val = toInteger ((shiftR val 23) .&. (0xff :: Word32))

-- | Extract float mantissa from 32 bit word
extractFloatMantissa :: Word32 -- ^ 32 bit word
  -> Integer -- ^ float mantissa
extractFloatMantissa val = if ((extractFloatExponent val) == 0) then toInteger (shiftL (val .&. (0x7fffff :: Word32)) 1) else toInteger ((val .&. (0x7fffff :: Word32)) .|. (0x800000 :: Word32))

-- | Read double from 64 bit word
deserializeDouble :: Word64 -- ^ 64 bit word
  -> Double -- ^ Double
deserializeDouble val
  | (val == 0x7ff0000000000000) = read "Infinity"
  | (val == 0xfff0000000000000) = read "-Infinity"
  | ((val >= 0x7ff0000000000001) && (val <= 0x7fffffffffffffff)) = read "NaN"
  | ((val >= 0xfff0000000000001) && (val <= 0xffffffffffffffff)) = read "NaN"
  | otherwise = encodeFloat ((extractDoubleSign val)*(extractDoubleMantissa val)) (fromIntegral ((extractDoubleExponent val) - 1075))

-- | Read double sign from 64 bit word
extractDoubleSign :: Word64 -- ^ 64 bit word
  -> Integer -- ^ 1 for positive, -1 for negative
extractDoubleSign val = if ((shiftR val 63) == 0) then (1 :: Integer) else (-1 :: Integer)

-- | Read double exponent from 64 bit word
extractDoubleExponent :: Word64 -- ^ 64 bit word
  -> Integer -- ^ double exponent + 1075
extractDoubleExponent val = toInteger ((shiftR val 52) .&. (0x7ff :: Word64))

-- | Extract double mantissa from 64 bit word
extractDoubleMantissa :: Word64 -- ^ 64 bit word
  -> Integer -- ^ double mantissa
extractDoubleMantissa val = if ((extractDoubleExponent val) == 0) then toInteger (shiftL (val .&. (0xfffffffffffff :: Word64)) 1) else toInteger ((val .&. (0xfffffffffffff :: Word64)) .|. (0x10000000000000 :: Word64))
