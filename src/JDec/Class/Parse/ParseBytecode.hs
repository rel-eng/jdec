module JDec.Class.Parse.ParseBytecode (
deserializeBytecode
) where 

import JDec.Bytecode.Raw.Instruction(Instruction(..))
import JDec.Class.Raw.ConstantPoolIndex (ConstantPoolIndex(ConstantPoolIndex))
import JDec.Bytecode.Raw.ArrayType(ArrayType(..))

import Data.Binary.Get(Get, isEmpty, getWord8, skip, getWord16be, getWord32be)
import Data.Map as Map (Map, empty, insert)
import Data.Int(Int8, Int16, Int32)
import Control.Monad(replicateM, when)

-- | Deserialize bytecode
deserializeBytecode :: Get (Map Integer Instruction) -- ^ Maps offsets to instructions
deserializeBytecode = do
  noInstructions <- isEmpty
  if noInstructions then return $! Map.empty else handleNextInstruction 0 Map.empty
  where
    handleNextInstruction currentOffset instructions = do
      opcode <- getWord8
      (instruction, instructionSize) <- handleOpcode opcode currentOffset
      noMoreInstructions <- isEmpty
      if noMoreInstructions then return $! Map.insert currentOffset instruction instructions else handleNextInstruction (currentOffset + instructionSize) (Map.insert currentOffset instruction instructions)
    handleOpcode opcode currentOffset =
      case opcode of
        0xfe -> return $! (Impdep1, 1)
        0xff -> return $! (Impdep2, 1)
        0xca -> return $! (Breakpoint, 1)
        0x32 -> return $! (Aaload, 1)
        0x53 -> return $! (Aastore, 1)
        0x01 -> return $! (AconstNull, 1)
        0x19 -> do
          index <- getWord8
          return $! (Aload (toInteger index), 2)
        0x2a -> return $! (AloadZero, 1)
        0x2b -> return $! (AloadOne, 1)
        0x2c -> return $! (AloadTwo, 1)
        0x2d -> return $! (AloadThree, 1)
        0xbd -> do
          index <- getWord16be
          return $! (Anewarray (ConstantPoolIndex (toInteger index)), 3)
        0xb0 -> return $! (Areturn, 1)
        0xbe -> return $! (Arraylength, 1)
        0x3a -> do
          index <- getWord8
          return $! (Astore (toInteger index), 2)
        0x4b -> return $! (AstoreZero, 1)
        0x4c -> return $! (AstoreOne, 1)
        0x4d -> return $! (AstoreTwo, 1)
        0x4e -> return $! (AstoreThree, 1)
        0xbf -> return $! (Athrow, 1)
        0x33 -> return $! (Baload, 1)
        0x54 -> return $! (Bastore, 1)
        0x10 -> do
          signedByte <- getWord8
          return $! (Bipush (toInteger ((fromIntegral signedByte) :: Int8)), 2)
        0x34 -> return $! (Caload, 1)
        0x55 -> return $! (Castore, 1)
        0xc0 -> do
          index <- getWord16be
          return $! (Checkcast (ConstantPoolIndex (toInteger index)), 3)
        0x90 -> return $! (DoubleToFloat, 1)
        0x8e -> return $! (DoubleToInt, 1)
        0x8f -> return $! (DoubleToLong, 1)
        0x63 -> return $! (Dadd, 1)
        0x31 -> return $! (Daload, 1)
        0x52 -> return $! (Dastore, 1)
        0x98 -> return $! (Dcmpg, 1)
        0x97 -> return $! (Dcmpl, 1)
        0x0e -> return $! (DconstZero, 1)
        0x0f -> return $! (DconstOne, 1)
        0x6f -> return $! (Ddiv, 1)
        0x18 -> do
          index <- getWord8
          return $! (Dload (toInteger index), 2)
        0x26 -> return $! (DloadZero, 1)
        0x27 -> return $! (DloadOne, 1)
        0x28 -> return $! (DloadTwo, 1)
        0x29 -> return $! (DloadThree, 1)
        0x6b -> return $! (Dmul, 1)
        0x77 -> return $! (Dneg, 1)
        0x73 -> return $! (Drem, 1)
        0xaf -> return $! (Dreturn, 1)
        0x39 -> do
          index <- getWord8
          return $! (Dstore (toInteger index), 2)
        0x47 -> return $! (DstoreZero, 1)
        0x48 -> return $! (DstoreOne, 1)
        0x49 -> return $! (DstoreTwo, 1)
        0x4a -> return $! (DstoreThree, 1)
        0x67 -> return $! (Dsub, 1)
        0x59 -> return $! (Dup, 1)
        0x5a -> return $! (DupXOne, 1)
        0x5b -> return $! (DupXTwo, 1)
        0x5c -> return $! (DupTwo, 1)
        0x5d -> return $! (DupTwoXOne, 1)
        0x5e -> return $! (DupTwoXTwo, 1)
        0x8d -> return $! (FloatToDouble, 1)
        0x8b -> return $! (FloatToInt, 1)
        0x8c -> return $! (FloatToLong, 1)
        0x62 -> return $! (Fadd, 1)
        0x30 -> return $! (Faload, 1)
        0x51 -> return $! (Fastore, 1)
        0x96 -> return $! (Fcmpg, 1)
        0x95 -> return $! (Fcmpl, 1)
        0x0b -> return $! (FconstZero, 1)
        0x0c -> return $! (FconstOne, 1)
        0x0d -> return $! (FconstTwo, 1)
        0x6e -> return $! (Fdiv, 1)
        0x17 -> do
          index <- getWord8
          return $! (Fload (toInteger index), 2)
        0x22 -> return $! (FloadZero, 1)
        0x23 -> return $! (FloadOne, 1)
        0x24 -> return $! (FloadTwo, 1)
        0x25 -> return $! (FloadThree, 1)
        0x6a -> return $! (Fmul, 1)
        0x76 -> return $! (Fneg, 1)
        0x72 -> return $! (Frem, 1)
        0xae -> return $! (Freturn, 1)
        0x38 -> do
          index <- getWord8
          return $! (Fstore (toInteger index), 2)
        0x43 -> return $! (FstoreZero, 1)
        0x44 -> return $! (FstoreOne, 1)
        0x45 -> return $! (FstoreTwo, 1)
        0x46 -> return $! (FstoreThree, 1)
        0x66 -> return $! (Fsub, 1)
        0xb4 -> do
          index <- getWord16be
          return $! (Getfield (ConstantPoolIndex (toInteger index)), 3)
        0xb2 -> do
          index <- getWord16be
          return $! (Getstatic (ConstantPoolIndex (toInteger index)), 3)
        0xa7 -> do
          branch <- getWord16be
          return $! (Goto (toInteger ((fromIntegral branch) :: Int16)), 3)
        0xc8 -> do
          branch <- getWord32be
          return $! (Gotow (toInteger ((fromIntegral branch) :: Int32)), 5)
        0x91 -> return $! (IntToByte, 1)
        0x92 -> return $! (IntToChar, 1)
        0x87 -> return $! (IntToDouble, 1)
        0x86 -> return $! (IntToFloat, 1)
        0x85 -> return $! (IntToLong, 1)
        0x93 -> return $! (IntToShort, 1)
        0x60 -> return $! (Iadd, 1)
        0x2e -> return $! (Iaload, 1)
        0x7e -> return $! (Iand, 1)
        0x4f -> return $! (Iastore, 1)
        0x02 -> return $! (IconstMinusOne, 1)
        0x03 -> return $! (IconstZero, 1)
        0x04 -> return $! (IconstOne, 1)
        0x05 -> return $! (IconstTwo, 1)
        0x06 -> return $! (IconstThree, 1)
        0x07 -> return $! (IconstFour, 1)
        0x08 -> return $! (IconstFive, 1)
        0x6c -> return $! (Idiv, 1)
        0xa5 -> do
          branch <- getWord16be
          return $! (IfAcmpeq (toInteger ((fromIntegral branch) :: Int16)), 3)
        0xa6 -> do
          branch <- getWord16be
          return $! (IfAcmpne (toInteger ((fromIntegral branch) :: Int16)), 3)
        0x9f -> do
          branch <- getWord16be
          return $! (IfIcmpeq (toInteger ((fromIntegral branch) :: Int16)), 3)
        0xa0 -> do
          branch <- getWord16be
          return $! (IfIcmpne (toInteger ((fromIntegral branch) :: Int16)), 3)
        0xa1 -> do
          branch <- getWord16be
          return $! (IfIcmplt (toInteger ((fromIntegral branch) :: Int16)), 3)
        0xa2 -> do
          branch <- getWord16be
          return $! (IfIcmpge (toInteger ((fromIntegral branch) :: Int16)), 3)
        0xa3 -> do
          branch <- getWord16be
          return $! (IfIcmpgt (toInteger ((fromIntegral branch) :: Int16)), 3)
        0xa4 -> do
          branch <- getWord16be
          return $! (IfIcmple (toInteger ((fromIntegral branch) :: Int16)), 3)
        0x99 -> do
          branch <- getWord16be
          return $! (Ifeq (toInteger ((fromIntegral branch) :: Int16)), 3)
        0x9a -> do
          branch <- getWord16be
          return $! (Ifne (toInteger ((fromIntegral branch) :: Int16)), 3)
        0x9b -> do
          branch <- getWord16be
          return $! (Iflt (toInteger ((fromIntegral branch) :: Int16)), 3)
        0x9c -> do
          branch <- getWord16be
          return $! (Ifge (toInteger ((fromIntegral branch) :: Int16)), 3)
        0x9d -> do
          branch <- getWord16be
          return $! (Ifgt (toInteger ((fromIntegral branch) :: Int16)), 3)
        0x9e -> do
          branch <- getWord16be
          return $! (Ifle (toInteger ((fromIntegral branch) :: Int16)), 3)
        0xc7 -> do
          branch <- getWord16be
          return $! (Ifnonnull (toInteger ((fromIntegral branch) :: Int16)), 3)
        0xc6 -> do
          branch <- getWord16be
          return $! (Ifnull (toInteger ((fromIntegral branch) :: Int16)), 3)
        0x84 -> do
          index <- getWord8
          signedConstVal <- getWord8
          return $! (Iinc (toInteger index) (toInteger ((fromIntegral signedConstVal) :: Int8)), 3)
        0x15 -> do
          index <- getWord8
          return $! (Iload (toInteger index), 2)
        0x1a -> return $! (IloadZero, 1)
        0x1b -> return $! (IloadOne, 1)
        0x1c -> return $! (IloadTwo, 1)
        0x1d -> return $! (IloadThree, 1)
        0x68 -> return $! (Imul, 1)
        0x74 -> return $! (Ineg, 1)
        0xc1 -> do
          index <- getWord16be
          return $! (Instanceof (ConstantPoolIndex (toInteger index)), 3)
        0xba -> do
          index <- getWord16be
          _ <- getWord8
          _ <- getWord8
          return $! (Invokedynamic (ConstantPoolIndex (toInteger index)), 5)
        0xb9 -> do
          index <- getWord16be
          count <- getWord8
          _ <- getWord8
          return $! (Invokeinterface (ConstantPoolIndex (toInteger index)) (toInteger count), 5)
        0xb7 -> do
          index <- getWord16be
          return $! (Invokespecial (ConstantPoolIndex (toInteger index)), 3)
        0xb8 -> do
          index <- getWord16be
          return $! (Invokestatic (ConstantPoolIndex (toInteger index)), 3)
        0xb6 -> do
          index <- getWord16be
          return $! (Invokevirtual (ConstantPoolIndex (toInteger index)), 3)
        0x80 -> return $! (Ior, 1)
        0x70 -> return $! (Irem, 1)
        0xac -> return $! (Ireturn, 1)
        0x78 -> return $! (Ishl, 1)
        0x7a -> return $! (Ishr, 1)
        0x36 -> do
          index <- getWord8
          return $! (Istore (toInteger index), 2)
        0x3b -> return $! (IstoreZero, 1)
        0x3c -> return $! (IstoreOne, 1)
        0x3d -> return $! (IstoreTwo, 1)
        0x3e -> return $! (IstoreThree, 1)
        0x64 -> return $! (Isub, 1)
        0x7c -> return $! (Iushr, 1)
        0x82 -> return $! (Ixor, 1)
        0xa8 -> do
          branch <- getWord16be
          return $! (Jsr (toInteger ((fromIntegral branch) :: Int16)), 3)
        0xc9 -> do
          branch <- getWord32be
          return $! (Jsrw (toInteger ((fromIntegral branch) :: Int32)), 5)
        0x8a -> return $! (LongToDouble, 1)
        0x89 -> return $! (LongToFloat, 1)
        0x88 -> return $! (LongToInt, 1)
        0x61 -> return $! (Ladd, 1)
        0x2f -> return $! (Laload, 1)
        0x7f -> return $! (Land, 1)
        0x50 -> return $! (Lastore, 1)
        0x94 -> return $! (Lcmp, 1)
        0x09 -> return $! (LconstZero, 1)
        0x0a -> return $! (LconstOne, 1)
        0x12 -> do
          index <- getWord8
          return $! (Ldc (ConstantPoolIndex (toInteger index)), 2)
        0x13 -> do
          index <- getWord16be
          return $! (Ldcw (ConstantPoolIndex (toInteger index)), 3)
        0x14 -> do
          index <- getWord16be
          return $! (LdcTwoW (ConstantPoolIndex (toInteger index)), 3)
        0x6d -> return $! (Ldiv, 1)
        0x16 -> do
          index <- getWord8
          return $! (Lload (toInteger index), 2)
        0x1e -> return $! (LloadZero, 1)
        0x1f -> return $! (LloadOne, 1)
        0x20 -> return $! (LloadTwo, 1)
        0x21 -> return $! (LloadThree, 1)
        0x69 -> return $! (Lmul, 1)
        0x75 -> return $! (Lneg, 1)
        0xab -> do
          skipToAlignment currentOffset
          defaultRaw <- getWord32be
          nPairsRaw <- getWord32be
          let nPairs = toInteger ((fromIntegral nPairsRaw) :: Int32)
          pairs <- readSwitchPairs nPairs
          return $! (Lookupswitch (toInteger ((fromIntegral defaultRaw) :: Int32)) pairs, 1 + (alignmentToSkip currentOffset) + 8 + 8 * (if nPairs >= 0 then nPairs else 0))
        0x81 -> return $! (Lor, 1)
        0x71 -> return $! (Lrem, 1)
        0xad -> return $! (Lreturn, 1)
        0x79 -> return $! (Lshl, 1)
        0x7b -> return $! (Lshr, 1)
        0x37 -> do
          index <- getWord8
          return $! (Lstore (toInteger index), 2)
        0x3f -> return $! (LstoreZero, 1)
        0x40 -> return $! (LstoreOne, 1)
        0x41 -> return $! (LstoreTwo, 1)
        0x42 -> return $! (LstoreThree, 1)
        0x65 -> return $! (Lsub, 1)
        0x7d -> return $! (Lushr, 1)
        0x83 -> return $! (Lxor, 1)
        0xc2 -> return $! (Monitorenter, 1)
        0xc3 -> return $! (Monitorexit, 1)
        0xc5 -> do
          index <- getWord16be
          dimensions <- getWord8
          return $! (Multianewarray (ConstantPoolIndex (toInteger index)) (toInteger dimensions), 4)
        0xbb -> do
          index <- getWord16be
          return $! (New (ConstantPoolIndex (toInteger index)), 3)
        0xbc -> do
          atype <- readArrayType
          return $! (NewArray atype, 2)
        0x00 -> return $! (Nop, 1)
        0x57 -> return $! (Pop, 1)
        0x58 -> return $! (PopTwo, 1)
        0xb5 -> do
          index <- getWord16be
          return $! (Putfield (ConstantPoolIndex (toInteger index)), 3)
        0xb3 -> do
          index <- getWord16be
          return $! (Putstatic (ConstantPoolIndex (toInteger index)), 3)
        0xa9 -> do
          index <- getWord8
          return $! (Ret (toInteger index), 2)
        0xb1 -> return $! (Return, 1)
        0x35 -> return $! (Saload, 1)
        0x56 -> return $! (Sastore, 1)
        0x11 -> do
          immediate <- getWord16be
          return $! (Sipush (toInteger ((fromIntegral immediate) :: Int16)), 3)
        0x5f -> return $! (Swap, 1)
        0xaa -> do
          skipToAlignment currentOffset
          defaultRaw <- getWord32be
          lowRaw <- getWord32be
          highRaw <- getWord32be
          let defaultOffset = toInteger ((fromIntegral defaultRaw) :: Int32)
          let low = toInteger ((fromIntegral lowRaw) :: Int32)
          let high = toInteger ((fromIntegral highRaw) :: Int32)
          when (low > high) (fail ("Invalid low and high index for tableswitch " ++ (show low) ++ ", " ++ (show high)))
          let nOffsets = high - low + 1
          jumpOffsets <- readSwitchOffsets nOffsets
          return $! (Tableswitch defaultOffset low high jumpOffsets, 1 + (alignmentToSkip currentOffset) + 12 + 4 * (if nOffsets >= 0 then nOffsets else 0))
        0xc4 -> do
          widenedOpcode <- getWord8
          case widenedOpcode of
            0x15 -> do
              index <- getWord16be
              return $! (WideIload (toInteger index), 4)
            0x17 -> do
              index <- getWord16be
              return $! (WideFload (toInteger index), 4)
            0x19 -> do
              index <- getWord16be
              return $! (WideAload (toInteger index), 4)
            0x16 -> do
              index <- getWord16be
              return $! (WideLload (toInteger index), 4)
            0x18 -> do
              index <- getWord16be
              return $! (WideDload (toInteger index), 4)
            0x36 -> do
              index <- getWord16be
              return $! (WideIstore (toInteger index), 4)
            0x38 -> do
              index <- getWord16be
              return $! (WideFstore (toInteger index), 4)
            0x3a -> do
              index <- getWord16be
              return $! (WideAstore (toInteger index), 4)
            0x37 -> do
              index <- getWord16be
              return $! (WideLstore (toInteger index), 4)
            0x39 -> do
              index <- getWord16be
              return $! (WideDstore (toInteger index), 4)
            0xa9 -> do
              index <- getWord16be
              return $! (WideRet (toInteger index), 4)
            0x84 -> do
              index <- getWord16be
              immediate <- getWord16be
              return $! (WideIinc (toInteger index) (toInteger ((fromIntegral immediate) :: Int16)), 6)
            _ -> fail ("Unknown opcode " ++ (show widenedOpcode) ++ " for wide prefix")
        _ -> fail ("Unknown opcode " ++ (show opcode))

-- | Bytes to skip to align offset on four byte boundary
alignmentToSkip :: Integer -- ^ Current offset
  -> Integer -- ^ Bytes to skip
alignmentToSkip currentOffset =
  case (currentOffset + 1) `mod` 4 of
    1 -> 3
    2 -> 2
    3 -> 1
    _ -> 0

-- | Skip bytes to align offset on four byte boundary
skipToAlignment :: Integer -> Get ()
skipToAlignment currentOffset = if (alignmentToSkip currentOffset) > 0 then skip (fromIntegral (alignmentToSkip currentOffset)) else return ()

-- | Read pairs of signed 32-bit values
readSwitchPairs :: Integer -- ^ Number of pairs to read
  -> Get [(Integer, Integer)] -- ^ Pairs of signed 32-bit values
readSwitchPairs nPairs = if nPairs > 0 then replicateM (fromIntegral nPairs) readSwitchPair else return $! []

-- | Read one pair of signed 32-bit values
readSwitchPair :: Get (Integer, Integer) -- ^ Pair of signed 32-bit values
readSwitchPair = do
  matchRaw <- getWord32be
  offsetRaw <- getWord32be
  return $! (toInteger ((fromIntegral matchRaw) :: Int32), toInteger ((fromIntegral offsetRaw) :: Int32))

-- | Read ArrayType
readArrayType :: Get ArrayType -- ^ ArrayType
readArrayType = do
  atype <- getWord8
  case atype of
    4 -> return $! ArrayTypeBoolean
    5 -> return $! ArrayTypeChar
    6 -> return $! ArrayTypeFloat
    7 -> return $! ArrayTypeDouble
    8 -> return $! ArrayTypeByte
    9 -> return $! ArrayTypeShort
    10 -> return $! ArrayTypeInt
    11 -> return $! ArrayTypeLong
    _ -> fail ("Unknown array type " ++ (show atype))

-- | Read signed 32-bit values
readSwitchOffsets :: Integer -- ^ Number of values to read
  -> Get [Integer] -- ^ Values
readSwitchOffsets nOffsets = if nOffsets > 0 then replicateM (fromIntegral nOffsets) readSwitchOffset else return $! []

-- | Read signed 32-bit value
readSwitchOffset :: Get Integer -- ^ Signed 32-bit value
readSwitchOffset = do
  offsetRaw <- getWord32be
  return $! (toInteger ((fromIntegral offsetRaw) :: Int32))
