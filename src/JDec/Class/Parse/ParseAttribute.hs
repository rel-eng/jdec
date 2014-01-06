module JDec.Class.Parse.ParseAttribute (
deserializeAttribute
) where 

import JDec.Class.Raw.Attribute (Attribute(ConstantValueAttribute, SyntheticAttribute, SignatureAttribute, DeprecatedAttribute, EnclosingMethodAttribute, SourceFileAttribute, StackMapTableAttribute, ExceptionsAttribute, CodeAttribute, InnerClassesAttribute, SourceDebugExtensionAttribute, LineNumberTableAttribute, LocalVariableTableAttribute, LocalVariableTypeTableAttribute, RuntimeVisibleAnnotationsAttribute, RuntimeInvisibleAnnotationsAttribute, RuntimeVisibleParameterAnnotationsAttribute, RuntimeInvisibleParameterAnnotationsAttribute, AnnotationDefaultAttribute, BootstrapMethodsAttribute))
import JDec.Class.Raw.ConstantPoolIndex(ConstantPoolIndex(ConstantPoolIndex))
import JDec.Class.Raw.ConstantPoolEntry (ConstantPoolEntry(UTF8ConstantPoolEntry))
import JDec.Class.Raw.StackMapFrame (StackMapFrame(SameFrame, SameLocalsOneStackItemFrame, SameLocalsOneStackItemFrameExtended, ChopFrame, SameFrameExtended, AppendFrame, FullFrame))
import JDec.Class.Raw.VerificationTypeInfo (VerificationTypeInfo(TopVariableInfo, IntegerVariableInfo, FloatVariableInfo, LongVariableInfo, DoubleVariableInfo, NullVariableInfo, UninitializedThisVariableInfo, ObjectVariableInfo, UninitializedVariableInfo))
import JDec.Class.Raw.ExceptionHandlerInfo (ExceptionHandlerInfo(ExceptionHandlerInfo))
import JDec.Class.Raw.InnerClassInfo (InnerClassInfo(InnerClassInfo))
import JDec.Class.Raw.InnerClassModifier (InnerClassModifier(PublicInnerClassModifier, PrivateInnerClassModifier, ProtectedInnerClassModifier, StaticInnerClassModifier, FinalInnerClassModifier, InterfaceInnerClassModifier, AbstractInnerClassModifier, SyntheticInnerClassModifier, AnnotationInnerClassModifier, EnumInnerClassModifier))
import JDec.Class.Parse.ParseEncodedString (parseString)
import JDec.Class.Raw.LineNumberInfo (LineNumberInfo(LineNumberInfo))
import JDec.Class.Raw.LocalVariableInfo (LocalVariableInfo(LocalVariableInfo))
import JDec.Class.Raw.LocalVariableTypeInfo (LocalVariableTypeInfo(LocalVariableTypeInfo))
import JDec.Class.Parse.ParseAnnotation(deserializeAnnotation, deserializeAnnotationElementValue)
import JDec.Class.Raw.BootstrapMethodInfo (BootstrapMethodInfo(BootstrapMethodInfo))
import JDec.Class.Parse.ParseBytecode (deserializeBytecode)

import Data.Binary.Get(Get, getWord16be, getWord32be, getLazyByteString, runGet, getWord8)
import Data.Map as Map (Map, lookup)
import Data.Text (unpack)
import Data.Int(Int64)
import Data.Word(Word16)
import Control.Monad(when, void, replicateM)
import Data.Maybe(catMaybes)
import Data.Set as Set (empty, insert)
import Data.Bits ((.&.))

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
            "InnerClasses" -> parseInnerClasses attributeLength
            "SourceDebugExtension" -> parseSourceDebugExtension attributeLength
            "LineNumberTable" -> parseLineNumberTable attributeLength
            "LocalVariableTable" -> parseLocalVariableTable attributeLength
            "LocalVariableTypeTable" -> parseLocalVariableTypeTable attributeLength
            "RuntimeVisibleAnnotations" -> parseRuntimeVisibleAnnotations attributeLength
            "RuntimeInvisibleAnnotations" -> parseRuntimeInvisibleAnnotations attributeLength
            "RuntimeVisibleParameterAnnotations" -> parseRuntimeVisibleParameterAnnotations attributeLength
            "RuntimeInvisibleParameterAnnotations" -> parseRuntimeInvisibleParameterAnnotations attributeLength
            "AnnotationDefault" -> parseAnnotationDefault attributeLength
            "BootstrapMethods" -> parseBootstrapMethods attributeLength
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
 when (attributeLength > 0) (void (getLazyByteString attributeLength))
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
 when (attributeLength > 0) (void (getLazyByteString attributeLength))
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
parseExceptions :: Int64 -- ^ Attribute length
  -> Get (Maybe Attribute) -- ^ Attribute, if any
parseExceptions attributeLength = 
  fmap (runGet deserializeExceptions) (getLazyByteString attributeLength)
  where
    deserializeExceptions = do
      exceptionsCount <- getWord16be
      exceptionIndexes <- replicateM (fromIntegral exceptionsCount) (fmap (ConstantPoolIndex . toInteger) (getWord16be))
      return $! Just (ExceptionsAttribute exceptionIndexes)

-- | Parse code attribute-specific data
parseCode :: Int64 -- ^ Attribute length
  -> Map ConstantPoolIndex ConstantPoolEntry -- ^ Constant pool
  -> Get (Maybe Attribute) -- ^ Attribute, if any
parseCode attributeLength constantPool =
  fmap (runGet deserializeCode) (getLazyByteString attributeLength)
  where
    deserializeCode = do
      maxStack <- getWord16be
      maxLocals <- getWord16be
      codeLength <- getWord32be
      instructions <- fmap (runGet deserializeBytecode) (getLazyByteString (fromIntegral codeLength))
      exceptionHandlersCount <- getWord16be
      exceptionHandlers <- replicateM (fromIntegral exceptionHandlersCount) deserializeExceptionHandlerInfo
      attributesCount <- getWord16be
      attributes <- replicateM (fromIntegral attributesCount) (deserializeAttribute constantPool)
      return $! Just (CodeAttribute (fromIntegral maxStack) (fromIntegral maxLocals) instructions exceptionHandlers (catMaybes attributes))
    deserializeExceptionHandlerInfo = do
      startPC <- getWord16be
      endPC <- getWord16be
      handlerPC <- getWord16be
      catchTypeIndex <- getWord16be
      return $! ExceptionHandlerInfo (toInteger startPC) (toInteger endPC) (toInteger handlerPC) (ConstantPoolIndex (toInteger catchTypeIndex))

-- | Parse inner classes attribute-specific data
parseInnerClasses :: Int64 -- ^ Attribute length
  -> Get (Maybe Attribute) -- ^ Attribute, if any
parseInnerClasses attributeLength =
  fmap (runGet deserializeInnerClasses) (getLazyByteString attributeLength)
  where
    deserializeInnerClasses = do
      classesCount <- getWord16be
      innerClasses <- replicateM (fromIntegral classesCount) deserializeInnerClassInfo
      return $! Just (InnerClassesAttribute innerClasses)
    deserializeInnerClassInfo = do
      innerClassInfoIndex <- getWord16be
      outerClassInfoIndex <- getWord16be
      innerNameIndex <- getWord16be
      innerClassAccessFlags <- getWord16be
      return $! InnerClassInfo (ConstantPoolIndex (toInteger innerClassInfoIndex)) (ConstantPoolIndex (toInteger outerClassInfoIndex)) (ConstantPoolIndex (toInteger innerNameIndex)) (deserializeInnerClassAccessFlags innerClassAccessFlags)
    deserializeInnerClassAccessFlags word = foldl (\s (x,y) -> if ((word .&. y) /= (0x0000 :: Word16)) then Set.insert x s else s) (Set.empty) [(PublicInnerClassModifier, 0x0001 :: Word16), (PrivateInnerClassModifier, 0x0002 :: Word16), (ProtectedInnerClassModifier, 0x0004 :: Word16), (StaticInnerClassModifier, 0x0008 :: Word16), (FinalInnerClassModifier, 0x0010 :: Word16), (InterfaceInnerClassModifier, 0x0200 :: Word16), (AbstractInnerClassModifier, 0x0400 :: Word16), (SyntheticInnerClassModifier, 0x1000 :: Word16), (AnnotationInnerClassModifier, 0x2000 :: Word16), (EnumInnerClassModifier, 0x4000 :: Word16)]

-- | Parse source debug extension attribute-specific data
parseSourceDebugExtension :: Int64 -- ^ Attribute length
  -> Get (Maybe Attribute) -- ^ Attribute, if any
parseSourceDebugExtension attributeLength = do
  bytes <- getLazyByteString attributeLength
  return $! Just (SourceDebugExtensionAttribute (parseString bytes))

-- | Parse line number table attribute-specific data
parseLineNumberTable :: Int64 -- ^ Attribute length
  -> Get (Maybe Attribute) -- ^ Attribute, if any
parseLineNumberTable attributeLength =
  fmap (runGet deserializeLineNumberTable) (getLazyByteString attributeLength)
  where
    deserializeLineNumberTable = do
      entriesCount <- getWord16be
      entries <- replicateM (fromIntegral entriesCount) deserializeLineNumberInfo
      return $! Just (LineNumberTableAttribute entries)
    deserializeLineNumberInfo = do
      startPC <- getWord16be
      lineNumber <- getWord16be
      return $! LineNumberInfo (toInteger startPC) (toInteger lineNumber)

-- | Parse local variable table attribute-specific data
parseLocalVariableTable :: Int64 -- ^ Attribute length
  -> Get (Maybe Attribute) -- ^ Attribute, if any
parseLocalVariableTable attributeLength =
  fmap (runGet deserializeLocalVaribleTable) (getLazyByteString attributeLength)
  where
    deserializeLocalVaribleTable = do
      entriesCount <- getWord16be
      entries <- replicateM (fromIntegral entriesCount) deserializeLocalVaribleInfo
      return $! Just (LocalVariableTableAttribute entries)
    deserializeLocalVaribleInfo = do
      localVariableStartPC <- getWord16be
      localVariableLength <- getWord16be
      localVariableNameIndex <- getWord16be
      localVariableDescriptorIndex <- getWord16be
      localVariableIndex <- getWord16be
      return $! LocalVariableInfo (toInteger localVariableStartPC) (toInteger localVariableLength) (ConstantPoolIndex (toInteger localVariableNameIndex)) (ConstantPoolIndex (toInteger localVariableDescriptorIndex)) (toInteger localVariableIndex)

-- | Parse local variable type table attribute-specific data
parseLocalVariableTypeTable :: Int64 -- ^ Attribute length
  -> Get (Maybe Attribute) -- ^ Attribute, if any
parseLocalVariableTypeTable attributeLength =
  fmap (runGet deserializeLocalVaribleTypeTable) (getLazyByteString attributeLength)
  where
    deserializeLocalVaribleTypeTable = do
      entriesCount <- getWord16be
      entries <- replicateM (fromIntegral entriesCount) deserializeLocalVaribleTypeInfo
      return $! Just (LocalVariableTypeTableAttribute entries)
    deserializeLocalVaribleTypeInfo = do
      localVariableStartPC <- getWord16be
      localVariableLength <- getWord16be
      localVariableNameIndex <- getWord16be
      localVariableSignatureIndex <- getWord16be
      localVariableIndex <- getWord16be
      return $! LocalVariableTypeInfo (toInteger localVariableStartPC) (toInteger localVariableLength) (ConstantPoolIndex (toInteger localVariableNameIndex)) (ConstantPoolIndex (toInteger localVariableSignatureIndex)) (toInteger localVariableIndex)

-- | Parse runtime visible annotations attribute-specific data
parseRuntimeVisibleAnnotations :: Int64 -- ^ Attribute length
  -> Get (Maybe Attribute) -- ^ Attribute, if any
parseRuntimeVisibleAnnotations attributeLength = 
  fmap (runGet deserializeRuntimeVisibleAnnotations) (getLazyByteString attributeLength)
  where
    deserializeRuntimeVisibleAnnotations = do
      annotationsCount <- getWord16be
      annotations <- replicateM (fromIntegral annotationsCount) deserializeAnnotation
      return $! Just (RuntimeVisibleAnnotationsAttribute annotations)

-- | Parse runtime invisible annotations attribute-specific data
parseRuntimeInvisibleAnnotations :: Int64 -- ^ Attribute length
  -> Get (Maybe Attribute) -- ^ Attribute, if any
parseRuntimeInvisibleAnnotations attributeLength = 
  fmap (runGet deserializeRuntimeInvisibleAnnotations) (getLazyByteString attributeLength)
  where
    deserializeRuntimeInvisibleAnnotations = do
      annotationsCount <- getWord16be
      annotations <- replicateM (fromIntegral annotationsCount) deserializeAnnotation
      return $! Just (RuntimeInvisibleAnnotationsAttribute annotations)

-- | Parse runtime visible parameter annotations attribute-specific data
parseRuntimeVisibleParameterAnnotations :: Int64 -- ^ Attribute length
  -> Get (Maybe Attribute) -- ^ Attribute, if any
parseRuntimeVisibleParameterAnnotations attributeLength = 
  fmap (runGet deserializeRuntimeVisibleAllParametersAnnotations) (getLazyByteString attributeLength)
  where
    deserializeRuntimeVisibleAllParametersAnnotations = do
      numParameters <- getWord8
      annotationsForParameters <- replicateM (fromIntegral numParameters) deserializeRuntimeVisibleOneParameterAnnotations
      return $! Just (RuntimeVisibleParameterAnnotationsAttribute annotationsForParameters)
    deserializeRuntimeVisibleOneParameterAnnotations = do
      annotationsCount <- getWord16be
      replicateM (fromIntegral annotationsCount) deserializeAnnotation

-- | Parse runtime invisible parameter annotations attribute-specific data
parseRuntimeInvisibleParameterAnnotations :: Int64 -- ^ Attribute length
  -> Get (Maybe Attribute) -- ^ Attribute, if any
parseRuntimeInvisibleParameterAnnotations attributeLength = 
  fmap (runGet deserializeRuntimeInvisibleAllParametersAnnotations) (getLazyByteString attributeLength)
  where
    deserializeRuntimeInvisibleAllParametersAnnotations = do
      numParameters <- getWord8
      annotationsForParameters <- replicateM (fromIntegral numParameters) deserializeRuntimeInvisibleOneParameterAnnotations
      return $! Just (RuntimeInvisibleParameterAnnotationsAttribute annotationsForParameters)
    deserializeRuntimeInvisibleOneParameterAnnotations = do
      annotationsCount <- getWord16be
      replicateM (fromIntegral annotationsCount) deserializeAnnotation

-- | Parse annotation default attribute-specific data
parseAnnotationDefault :: Int64 -- ^ Attribute length
  -> Get (Maybe Attribute) -- ^ Attribute, if any
parseAnnotationDefault attributeLength = 
  fmap (runGet deserializeAnnotationDefault) (getLazyByteString attributeLength)
  where
    deserializeAnnotationDefault = do
      defaultValue <- deserializeAnnotationElementValue
      return $! Just (AnnotationDefaultAttribute defaultValue)

-- | Parse bootstrap methods attribute-specific data
parseBootstrapMethods :: Int64 -- ^ Attribute length
  -> Get (Maybe Attribute) -- ^ Attribute, if any
parseBootstrapMethods attributeLength = 
  fmap (runGet deserializeBootstrapMethods) (getLazyByteString attributeLength)
  where
    deserializeBootstrapMethods = do
      bootstrapMethodsCount <- getWord16be
      bootstrapMethods <- replicateM (fromIntegral bootstrapMethodsCount) deserializeBootstrapMethodInfo
      return $! Just (BootstrapMethodsAttribute bootstrapMethods)
    deserializeBootstrapMethodInfo = do
      bootstrapMethodRef <- getWord16be
      argumentsCount <- getWord16be
      bootstrapArguments <- replicateM (fromIntegral argumentsCount) (fmap (ConstantPoolIndex . toInteger) (getWord16be))
      return $! BootstrapMethodInfo (ConstantPoolIndex (toInteger bootstrapMethodRef)) bootstrapArguments

-- | Skip attribute
skipAttribute :: Int64 -- ^ Attribute length
  -> Get (Maybe Attribute) -- ^ Nothing
skipAttribute attributeLength = do
  _ <- getLazyByteString attributeLength
  return $! Nothing
