module JDec.Class.Parse.ParseAttribute (
deserializeAttribute
) where 

import JDec.Class.Raw.Attribute (Attribute(ConstantValueAttribute, SyntheticAttribute, SignatureAttribute, DeprecatedAttribute, EnclosingMethodAttribute, SourceFileAttribute, StackMapTableAttribute, ExceptionsAttribute, CodeAttribute))
import JDec.Class.Raw.ConstantPoolIndex(ConstantPoolIndex(ConstantPoolIndex))
import JDec.Class.Raw.ConstantPoolEntry (ConstantPoolEntry(UTF8ConstantPoolEntry))
import JDec.Class.Raw.StackMapFrame (StackMapFrame(SameFrame, SameLocalsOneStackItemFrame, SameLocalsOneStackItemFrameExtended, ChopFrame, SameFrameExtended, AppendFrame, FullFrame))
import JDec.Class.Raw.VerificationTypeInfo (VerificationTypeInfo(TopVariableInfo, IntegerVariableInfo, FloatVariableInfo, LongVariableInfo, DoubleVariableInfo, NullVariableInfo, UninitializedThisVariableInfo, ObjectVariableInfo, UninitializedVariableInfo))
import JDec.Class.Raw.ExceptionHandlerInfo (ExceptionHandlerInfo(ExceptionHandlerInfo))

import Data.Binary.Get(Get, getWord16be, getWord32be, getLazyByteString, runGet, getWord8)
import Data.Map as Map (Map, lookup)
import Data.Text (unpack)
import Data.Int(Int64)
import Control.Monad(when, void, replicateM)
import Data.Maybe(catMaybes)

-- | Deserialize one attribute
deserializeAttribute :: Map ConstantPoolIndex ConstantPoolEntry -- ^ Constant pool
  -> Get (Maybe Attribute) -- ^ Attribute, if any
deserializeAttribute constantPool = do
  nameIndex <- getWord16be
  attributeLength <- getWord32be
  parseAttribute constantPool (ConstantPoolIndex (toInteger nameIndex)) (fromIntegral attributeLength)

-- | Parse attribute-specific data
parseAttribute :: Map ConstantPoolIndex ConstantPoolEntry -- ^ Constant pool
  -> ConstantPoolIndex -- ^ Attribute name index
  -> Int64 -- ^ Attribute length
  -> Get (Maybe Attribute) -- ^ Attribute, if any
parseAttribute constantPool nameIndex attributeLength =
  case Map.lookup nameIndex constantPool of
    Just entry -> 
      case entry of
        UTF8ConstantPoolEntry t ->
          case (unpack t) of
            "ConstantValue" -> if attributeLength >= 2 then parseConstantValue attributeLength else skipAttribute attributeLength
            "Synthetic" -> parseSynthetic attributeLength
            "Signature" -> if attributeLength >= 2 then parseSignatureValue attributeLength else skipAttribute attributeLength
            "Deprecated" -> parseDeprecated attributeLength
            "EnclosingMethod" -> if attributeLength >= 4 then parseEnclosingMethod attributeLength else skipAttribute attributeLength
            "SourceFile" -> if attributeLength >= 2 then parseSourceFile attributeLength else skipAttribute attributeLength
            "StackMapTable" -> parseStackMapTable attributeLength
            "Exceptions" -> parseExceptions attributeLength
            "Code" -> parseCode attributeLength constantPool
            _ -> skipAttribute attributeLength
        _ -> skipAttribute attributeLength
    Nothing -> skipAttribute attributeLength

-- | Parse constant value attribute-specific data
parseConstantValue :: Int64 -- ^ Attribute length
  -> Get (Maybe Attribute) -- ^ Attribute, if any
parseConstantValue attributeLength = do
 constantIndex <- getWord16be
 when (attributeLength > 2) (void (getLazyByteString (attributeLength - 2)))
 return $! Just (ConstantValueAttribute (ConstantPoolIndex (toInteger constantIndex)))

-- | Parse synthetic attribute-specific data
parseSynthetic :: Int64 -- ^ Attribute length
  -> Get (Maybe Attribute) -- ^ Attribute, if any
parseSynthetic attributeLength = do
 when (attributeLength > 0) (void (getLazyByteString (attributeLength)))
 return $! Just SyntheticAttribute

-- | Parse signature attribute-specific data
parseSignatureValue :: Int64 -- ^ Attribute length
  -> Get (Maybe Attribute) -- ^ Attribute, if any
parseSignatureValue attributeLength = do
 signatureIndex <- getWord16be
 when (attributeLength > 2) (void (getLazyByteString (attributeLength - 2)))
 return $! Just (SignatureAttribute (ConstantPoolIndex (toInteger signatureIndex)))

-- | Parse deprecated attribute-specific data
parseDeprecated :: Int64 -- ^ Attribute length
  -> Get (Maybe Attribute) -- ^ Attribute, if any
parseDeprecated attributeLength = do
 when (attributeLength > 0) (void (getLazyByteString (attributeLength)))
 return $! Just DeprecatedAttribute

-- | Parse enclosing method attribute-specific data
parseEnclosingMethod :: Int64 -- ^ Attribute length
  -> Get (Maybe Attribute) -- ^ Attribute, if any
parseEnclosingMethod attributeLength = do
 classIndex <- getWord16be
 methodIndex <- getWord16be
 when (attributeLength > 4) (void (getLazyByteString (attributeLength - 4)))
 return $! Just (EnclosingMethodAttribute (ConstantPoolIndex (toInteger classIndex)) (ConstantPoolIndex (toInteger methodIndex)))

-- | Parse source file attribute-specific data
parseSourceFile :: Int64 -- ^ Attribute length
  -> Get (Maybe Attribute) -- ^ Attribute, if any
parseSourceFile attributeLength = do
 sourceFileIndex <- getWord16be
 when (attributeLength > 2) (void (getLazyByteString (attributeLength - 2)))
 return $! Just (SourceFileAttribute (ConstantPoolIndex (toInteger sourceFileIndex)))

-- | Parse stack map table attribute-specific data
parseStackMapTable :: Int64 -- ^ Attribute length
  -> Get (Maybe Attribute) -- ^ Attribute, if any
parseStackMapTable attributeLength = 
  fmap (runGet deserializeStackMapTable) (getLazyByteString attributeLength)
  where
    deserializeStackMapTable = do
      entriesCount <- getWord16be
      frames <- replicateM (fromIntegral entriesCount) deserializeFrame
      return $! Just (StackMapTableAttribute frames)
    deserializeFrame = do
      tag <- getWord8
      parseFrame tag
    parseFrame tag
      | tag >= 0 && tag <= 63 = return $! SameFrame (toInteger tag)
      | tag >= 64 && tag <= 127 = do
        verificationTypeInfo <- deserializeVerificationTypeInfo
        return $! SameLocalsOneStackItemFrame (toInteger (tag - 64)) verificationTypeInfo
      | tag == 247 = do
        offsetDelta <- getWord16be
        verificationTypeInfo <- deserializeVerificationTypeInfo
        return $! SameLocalsOneStackItemFrameExtended (toInteger offsetDelta) verificationTypeInfo
      | tag >= 248 && tag <= 250 = do
        offsetDelta <- getWord16be
        return $! ChopFrame (toInteger (251 - tag)) (toInteger offsetDelta)
      | tag == 251 = do
        offsetDelta <- getWord16be
        return $! SameFrameExtended (toInteger offsetDelta)
      | tag >= 252 && tag <= 254 = do
        offsetDelta <- getWord16be
        locals <- replicateM (fromIntegral (tag - 251)) deserializeVerificationTypeInfo
        return $! AppendFrame (toInteger offsetDelta) locals
      | tag == 255 = do
        offsetDelta <- getWord16be
        numberOfLocals <- getWord16be
        locals <- replicateM (fromIntegral numberOfLocals) deserializeVerificationTypeInfo
        numberOfStackItems <- getWord16be
        stack <- replicateM (fromIntegral numberOfStackItems) deserializeVerificationTypeInfo
        return $! FullFrame (toInteger offsetDelta) locals stack
      | otherwise = fail ("Unknown frame type " ++ (show tag))
    deserializeVerificationTypeInfo = do
      typeTag <- getWord8
      case typeTag of
        0 -> return $! TopVariableInfo
        1 -> return $! IntegerVariableInfo
        2 -> return $! FloatVariableInfo
        3 -> return $! DoubleVariableInfo
        4 -> return $! LongVariableInfo
        5 -> return $! NullVariableInfo
        6 -> return $! UninitializedThisVariableInfo
        7 -> do
          classIndex <- getWord16be
          return $! ObjectVariableInfo (ConstantPoolIndex (toInteger classIndex))
        8 -> do
          codeOffset <- getWord16be
          return $! UninitializedVariableInfo (toInteger codeOffset)
        _ -> fail ("Unknown variable info type " ++ (show typeTag))

-- | Parse exceptions attribute-specific data
parseExceptions :: Int64  -- ^ Attribute length
  -> Get (Maybe Attribute) -- ^ Attribute, if any
parseExceptions attributeLength = 
  fmap (runGet deserializeExceptions) (getLazyByteString attributeLength)
  where
    deserializeExceptions = do
      exceptionsCount <- getWord16be
      exceptionIndexes <- replicateM (fromIntegral exceptionsCount) (fmap (ConstantPoolIndex . toInteger) (getWord16be))
      return $! Just (ExceptionsAttribute exceptionIndexes)

parseCode :: Int64
  -> Map ConstantPoolIndex ConstantPoolEntry
  -> Get (Maybe Attribute)
parseCode attributeLength constantPool =
  fmap (runGet deserializeCode) (getLazyByteString attributeLength)
  where
    deserializeCode = do
      maxStack <- getWord16be
      maxLocals <- getWord16be
      codeLength <- getWord32be
      code <- getLazyByteString (fromIntegral codeLength)
      exceptionHandlersCount <- getWord16be
      exceptionHandlers <- replicateM (fromIntegral exceptionHandlersCount) deserializeExceptionHandlerInfo
      attributesCount <- getWord16be
      attributes <- replicateM (fromIntegral attributesCount) (deserializeAttribute constantPool)
      return $! Just (CodeAttribute (fromIntegral maxStack) (fromIntegral maxLocals) [] exceptionHandlers (catMaybes attributes))
    deserializeExceptionHandlerInfo = do
      startPC <- getWord16be
      endPC <- getWord16be
      handlerPC <- getWord16be
      catchTypeIndex <- getWord16be
      return $! ExceptionHandlerInfo (toInteger startPC) (toInteger endPC) (toInteger handlerPC) (ConstantPoolIndex (toInteger catchTypeIndex))

-- | Skip attribute
skipAttribute :: Int64 -- ^ Attribute length
  -> Get (Maybe Attribute) -- ^ Nothing
skipAttribute attributeLength = do
  _ <- getLazyByteString attributeLength
  return $! Nothing
