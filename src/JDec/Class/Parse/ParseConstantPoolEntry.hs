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
import JDec.Class.Parse.ParseEncodedString (parseString)

import Data.Binary.Get(Get,getWord8,getWord16be,getWord32be,getLazyByteString)
import Data.Word(Word32, Word64)
import Data.Int(Int32, Int64)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))

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
