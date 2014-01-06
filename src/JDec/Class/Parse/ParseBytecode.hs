module JDec.Class.Parse.ParseBytecode (
deserializeBytecode
) where 

import JDec.Bytecode.Raw.Instruction(Instruction(..))
import JDec.Class.Raw.ConstantPoolIndex (ConstantPoolIndex(ConstantPoolIndex))
import JDec.Bytecode.Raw.ArrayType(ArrayType(..))

import Data.Binary.Get(Get, isEmpty, getWord8, skip)
import Data.Map as Map (Map, empty, insert)
import Data.Bits (shiftL, (.|.))
import Data.Word(Word8, Word16, Word32)
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
          indexByte1 <- getWord8
          indexByte2 <- getWord8
          return $! (Anewarray (bytesToConstantPoolIndex indexByte1 indexByte2), 3)
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
          indexByte1 <- getWord8
          indexByte2 <- getWord8
          return $! (Checkcast (bytesToConstantPoolIndex indexByte1 indexByte2), 3)
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
          indexByte1 <- getWord8
          indexByte2 <- getWord8
          return $! (Getfield (bytesToConstantPoolIndex indexByte1 indexByte2), 3)
        0xb2 -> do
          indexByte1 <- getWord8
          indexByte2 <- getWord8
          return $! (Getstatic (bytesToConstantPoolIndex indexByte1 indexByte2), 3)
        0xa7 -> do
          branchByte1 <- getWord8
          branchByte2 <- getWord8
          return $! (Goto (bytesToShortOffset branchByte1 branchByte2), 3)
        0xc8 -> do
          branchByte1 <- getWord8
          branchByte2 <- getWord8
          branchByte3 <- getWord8
          branchByte4 <- getWord8
          return $! (Gotow (bytesToIntegerOffset branchByte1 branchByte2 branchByte3 branchByte4), 5)
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
          branchByte1 <- getWord8
          branchByte2 <- getWord8
          return $! (IfAcmpeq (bytesToShortOffset branchByte1 branchByte2), 3)
        0xa6 -> do
          branchByte1 <- getWord8
          branchByte2 <- getWord8
          return $! (IfAcmpne (bytesToShortOffset branchByte1 branchByte2), 3)
        0x9f -> do
          branchByte1 <- getWord8
          branchByte2 <- getWord8
          return $! (IfIcmpeq (bytesToShortOffset branchByte1 branchByte2), 3)
        0xa0 -> do
          branchByte1 <- getWord8
          branchByte2 <- getWord8
          return $! (IfIcmpne (bytesToShortOffset branchByte1 branchByte2), 3)
        0xa1 -> do
          branchByte1 <- getWord8
          branchByte2 <- getWord8
          return $! (IfIcmplt (bytesToShortOffset branchByte1 branchByte2), 3)
        0xa2 -> do
          branchByte1 <- getWord8
          branchByte2 <- getWord8
          return $! (IfIcmpge (bytesToShortOffset branchByte1 branchByte2), 3)
        0xa3 -> do
          branchByte1 <- getWord8
          branchByte2 <- getWord8
          return $! (IfIcmpgt (bytesToShortOffset branchByte1 branchByte2), 3)
        0xa4 -> do
          branchByte1 <- getWord8
          branchByte2 <- getWord8
          return $! (IfIcmple (bytesToShortOffset branchByte1 branchByte2), 3)
        0x99 -> do
          branchByte1 <- getWord8
          branchByte2 <- getWord8
          return $! (Ifeq (bytesToShortOffset branchByte1 branchByte2), 3)
        0x9a -> do
          branchByte1 <- getWord8
          branchByte2 <- getWord8
          return $! (Ifne (bytesToShortOffset branchByte1 branchByte2), 3)
        0x9b -> do
          branchByte1 <- getWord8
          branchByte2 <- getWord8
          return $! (Iflt (bytesToShortOffset branchByte1 branchByte2), 3)
        0x9c -> do
          branchByte1 <- getWord8
          branchByte2 <- getWord8
          return $! (Ifge (bytesToShortOffset branchByte1 branchByte2), 3)
        0x9d -> do
          branchByte1 <- getWord8
          branchByte2 <- getWord8
          return $! (Ifgt (bytesToShortOffset branchByte1 branchByte2), 3)
        0x9e -> do
          branchByte1 <- getWord8
          branchByte2 <- getWord8
          return $! (Ifle (bytesToShortOffset branchByte1 branchByte2), 3)
        0xc7 -> do
          branchByte1 <- getWord8
          branchByte2 <- getWord8
          return $! (Ifnonnull (bytesToShortOffset branchByte1 branchByte2), 3)
        0xc6 -> do
          branchByte1 <- getWord8
          branchByte2 <- getWord8
          return $! (Ifnull (bytesToShortOffset branchByte1 branchByte2), 3)
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
          indexByte1 <- getWord8
          indexByte2 <- getWord8
          return $! (Instanceof (bytesToConstantPoolIndex indexByte1 indexByte2), 3)
        0xba -> do
          indexByte1 <- getWord8
          indexByte2 <- getWord8
          _ <- getWord8
          _ <- getWord8
          return $! (Invokedynamic (bytesToConstantPoolIndex indexByte1 indexByte2), 5)
        0xb9 -> do
          indexByte1 <- getWord8
          indexByte2 <- getWord8
          count <- getWord8
          _ <- getWord8
          return $! (Invokeinterface (bytesToConstantPoolIndex indexByte1 indexByte2) (toInteger count), 5)
        0xb7 -> do
          indexByte1 <- getWord8
          indexByte2 <- getWord8
          return $! (Invokespecial (bytesToConstantPoolIndex indexByte1 indexByte2), 3)
        0xb8 -> do
          indexByte1 <- getWord8
          indexByte2 <- getWord8
          return $! (Invokestatic (bytesToConstantPoolIndex indexByte1 indexByte2), 3)
        0xb6 -> do
          indexByte1 <- getWord8
          indexByte2 <- getWord8
          return $! (Invokevirtual (bytesToConstantPoolIndex indexByte1 indexByte2), 3)
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
          branchByte1 <- getWord8
          branchByte2 <- getWord8
          return $! (Jsr (bytesToShortOffset branchByte1 branchByte2), 3)
        0xc9 -> do
          branchByte1 <- getWord8
          branchByte2 <- getWord8
          branchByte3 <- getWord8
          branchByte4 <- getWord8
          return $! (Jsrw (bytesToIntegerOffset branchByte1 branchByte2 branchByte3 branchByte4), 5)
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
          indexByte1 <- getWord8
          indexByte2 <- getWord8
          return $! (Ldcw (bytesToConstantPoolIndex indexByte1 indexByte2), 3)
        0x14 -> do
          indexByte1 <- getWord8
          indexByte2 <- getWord8
          return $! (LdcTwoW (bytesToConstantPoolIndex indexByte1 indexByte2), 3)
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
          defaultByte1 <- getWord8
          defaultByte2 <- getWord8
          defaultByte3 <- getWord8
          defaultByte4 <- getWord8
          nPairs1 <- getWord8
          nPairs2 <- getWord8
          nPairs3 <- getWord8
          nPairs4 <- getWord8
          let nPairs = bytesToIntegerOffset nPairs1 nPairs2 nPairs3 nPairs4
          pairs <- readSwitchPairs nPairs
          return $! (Lookupswitch (bytesToIntegerOffset defaultByte1 defaultByte2 defaultByte3 defaultByte4) pairs, 1 + (alignmentToSkip currentOffset) + 8 + 8 * (if nPairs >= 0 then nPairs else 0))
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
          indexByte1 <- getWord8
          indexByte2 <- getWord8
          dimensions <- getWord8
          return $! (Multianewarray (bytesToConstantPoolIndex indexByte1 indexByte2) (toInteger dimensions), 4)
        0xbb -> do
          indexByte1 <- getWord8
          indexByte2 <- getWord8
          return $! (New (bytesToConstantPoolIndex indexByte1 indexByte2), 3)
        0xbc -> do
          atype <- readArrayType
          return $! (NewArray atype, 2)
        0x00 -> return $! (Nop, 1)
        0x57 -> return $! (Pop, 1)
        0x58 -> return $! (PopTwo, 1)
        0xb5 -> do
          indexByte1 <- getWord8
          indexByte2 <- getWord8
          return $! (Putfield (bytesToConstantPoolIndex indexByte1 indexByte2), 3)
        0xb3 -> do
          indexByte1 <- getWord8
          indexByte2 <- getWord8
          return $! (Putstatic (bytesToConstantPoolIndex indexByte1 indexByte2), 3)
        0xa9 -> do
          index <- getWord8
          return $! (Ret (toInteger index), 2)
        0xb1 -> return $! (Return, 1)
        0x35 -> return $! (Saload, 1)
        0x56 -> return $! (Sastore, 1)
        0x11 -> do
          immediateByte1 <- getWord8
          immediateByte2 <- getWord8
          return $! (Sipush (bytesToShortOffset immediateByte1 immediateByte2), 3)
        0x5f -> return $! (Swap, 1)
        0xaa -> do
          skipToAlignment currentOffset
          defaultByte1 <- getWord8
          defaultByte2 <- getWord8
          defaultByte3 <- getWord8
          defaultByte4 <- getWord8
          lowByte1 <- getWord8
          lowByte2 <- getWord8
          lowByte3 <- getWord8
          lowByte4 <- getWord8
          highByte1 <- getWord8
          highByte2 <- getWord8
          highByte3 <- getWord8
          highByte4 <- getWord8
          let defaultOffset = bytesToIntegerOffset defaultByte1 defaultByte2 defaultByte3 defaultByte4
          let low = bytesToIntegerOffset lowByte1 lowByte2 lowByte3 lowByte4
          let high = bytesToIntegerOffset highByte1 highByte2 highByte3 highByte4
          when (low > high) (fail ("Invalid low and high index for tableswitch " ++ (show low) ++ ", " ++ (show high)))
          let nOffsets = high - low + 1
          jumpOffsets <- readSwitchOffsets nOffsets
          return $! (Tableswitch defaultOffset low high jumpOffsets, 1 + (alignmentToSkip currentOffset) + 12 + 4 * (if nOffsets >= 0 then nOffsets else 0))
        0xc4 -> do
          widenedOpcode <- getWord8
          case widenedOpcode of
            0x15 -> do
              indexByte1 <- getWord8
              indexByte2 <- getWord8
              return $! (WideIload (bytesToShortIndex indexByte1 indexByte2), 4)
            0x17 -> do
              indexByte1 <- getWord8
              indexByte2 <- getWord8
              return $! (WideFload (bytesToShortIndex indexByte1 indexByte2), 4)
            0x19 -> do
              indexByte1 <- getWord8
              indexByte2 <- getWord8
              return $! (WideAload (bytesToShortIndex indexByte1 indexByte2), 4)
            0x16 -> do
              indexByte1 <- getWord8
              indexByte2 <- getWord8
              return $! (WideLload (bytesToShortIndex indexByte1 indexByte2), 4)
            0x18 -> do
              indexByte1 <- getWord8
              indexByte2 <- getWord8
              return $! (WideDload (bytesToShortIndex indexByte1 indexByte2), 4)
            0x36 -> do
              indexByte1 <- getWord8
              indexByte2 <- getWord8
              return $! (WideIstore (bytesToShortIndex indexByte1 indexByte2), 4)
            0x38 -> do
              indexByte1 <- getWord8
              indexByte2 <- getWord8
              return $! (WideFstore (bytesToShortIndex indexByte1 indexByte2), 4)
            0x3a -> do
              indexByte1 <- getWord8
              indexByte2 <- getWord8
              return $! (WideAstore (bytesToShortIndex indexByte1 indexByte2), 4)
            0x37 -> do
              indexByte1 <- getWord8
              indexByte2 <- getWord8
              return $! (WideLstore (bytesToShortIndex indexByte1 indexByte2), 4)
            0x39 -> do
              indexByte1 <- getWord8
              indexByte2 <- getWord8
              return $! (WideDstore (bytesToShortIndex indexByte1 indexByte2), 4)
            0xa9 -> do
              indexByte1 <- getWord8
              indexByte2 <- getWord8
              return $! (WideRet (bytesToShortIndex indexByte1 indexByte2), 4)
            0x84 -> do
              indexByte1 <- getWord8
              indexByte2 <- getWord8
              immediateByte1 <- getWord8
              immediateByte2 <- getWord8
              return $! (WideIinc (bytesToShortIndex indexByte1 indexByte2) (bytesToShortOffset immediateByte1 immediateByte2), 6)
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
  match1 <- getWord8
  match2 <- getWord8
  match3 <- getWord8
  match4 <- getWord8
  offset1 <- getWord8
  offset2 <- getWord8
  offset3 <- getWord8
  offset4 <- getWord8
  return $! (bytesToIntegerOffset match1 match2 match3 match4, bytesToIntegerOffset offset1 offset2 offset3 offset4)

-- | Make a ConstantPoolIndex from two unsigned bytes
bytesToConstantPoolIndex :: Word8 -- ^ First byte
  -> Word8 -- ^ Second byte
  -> ConstantPoolIndex -- ^ ConstantPoolIndex
bytesToConstantPoolIndex indexByte1 indexByte2 = ConstantPoolIndex (toInteger ((shiftL ((fromIntegral indexByte1) :: Word16) 8) .|. (fromIntegral indexByte2)))

-- | Make a signed short from two unsigned bytes
bytesToShortOffset :: Word8 -- ^ First byte
  -> Word8 -- ^ Second byte
  -> Integer -- ^ Value of the signed short
bytesToShortOffset branchByte1 branchByte2 = toInteger ((fromIntegral ((shiftL ((fromIntegral branchByte1) :: Word16) 8) .|. (fromIntegral branchByte2))) :: Int16)

-- | Make a signed int from four unsigned bytes
bytesToIntegerOffset :: Word8 -- ^ First byte
  -> Word8 -- ^ Second byte
  -> Word8 -- ^ Third byte
  -> Word8 -- ^ Fourth byte
  -> Integer -- ^ Value of the signed int
bytesToIntegerOffset branchByte1 branchByte2 branchByte3 branchByte4 = toInteger ((fromIntegral ((((shiftL ((fromIntegral branchByte1) :: Word32) 24) .|. (shiftL ((fromIntegral branchByte2) :: Word32) 16)) .|. (shiftL ((fromIntegral branchByte3) :: Word32) 8)) .|. (fromIntegral branchByte4))) :: Int32)

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
  offset1 <- getWord8
  offset2 <- getWord8
  offset3 <- getWord8
  offset4 <- getWord8
  return $! (bytesToIntegerOffset offset1 offset2 offset3 offset4)

-- | Make unsigned short from two unsigned bytes
bytesToShortIndex :: Word8 -- ^ First byte
  -> Word8 -- ^ Second byte
  -> Integer -- ^ Value of the signed short
bytesToShortIndex indexByte1 indexByte2 = toInteger ((shiftL ((fromIntegral indexByte1) :: Word16) 8) .|. (fromIntegral indexByte2))
