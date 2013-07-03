module JDec.Class.Parse.ParseAnnotation (
deserializeAnnotation,
deserializeAnnotationElementValue
) where  

import JDec.Class.Raw.Annotation (Annotation(Annotation), AnnotationElementValue(AnnotationElementConstByteValue, AnnotationElementConstCharValue, AnnotationElementConstDoubleValue, AnnotationElementConstFloatValue, AnnotationElementConstIntegerValue, AnnotationElementConstLongValue, AnnotationElementConstShortValue, AnnotationElementConstBooleanValue, AnnotationElementConstStringValue, AnnotationElementEnumConstValue, AnnotationElementClassValue, AnnotationElementAnnotationValue, AnnotationElementArrayValue))
import JDec.Class.Raw.ConstantPoolIndex(ConstantPoolIndex(ConstantPoolIndex))

import Data.Binary.Get(Get, getWord16be, getWord8)
import Control.Monad(replicateM)

-- | Deserialize one annotation
deserializeAnnotation ::  Get Annotation -- ^ Annotation
deserializeAnnotation = do
  typeIndex <- getWord16be
  entriesCount <- getWord16be
  entries <- replicateM (fromIntegral entriesCount) deserializeAnnotationElementValuePair
  return $! Annotation (ConstantPoolIndex (toInteger typeIndex)) entries

-- | Deserialize one annotation element name and value pair
deserializeAnnotationElementValuePair :: Get (ConstantPoolIndex, AnnotationElementValue) -- ^ Annotation element name and value pair
deserializeAnnotationElementValuePair = do
  elementNameIndex <- getWord16be
  elementValue <- deserializeAnnotationElementValue
  return $! ((ConstantPoolIndex (toInteger elementNameIndex)), elementValue)

-- | Deserialize one annotation element value
deserializeAnnotationElementValue :: Get AnnotationElementValue -- ^ Annotation element value
deserializeAnnotationElementValue = do
  tag <- getWord8
  case tag of
    0x42 -> do
      constValueIndex <- getWord16be
      return $! AnnotationElementConstByteValue (ConstantPoolIndex (toInteger constValueIndex))
    0x43 -> do
      constValueIndex <- getWord16be
      return $! AnnotationElementConstCharValue (ConstantPoolIndex (toInteger constValueIndex))
    0x44 -> do
      constValueIndex <- getWord16be
      return $! AnnotationElementConstDoubleValue (ConstantPoolIndex (toInteger constValueIndex))
    0x46 -> do
      constValueIndex <- getWord16be
      return $! AnnotationElementConstFloatValue (ConstantPoolIndex (toInteger constValueIndex))
    0x49 -> do
      constValueIndex <- getWord16be
      return $! AnnotationElementConstIntegerValue (ConstantPoolIndex (toInteger constValueIndex))
    0x4A -> do
      constValueIndex <- getWord16be
      return $! AnnotationElementConstLongValue (ConstantPoolIndex (toInteger constValueIndex))
    0x53 -> do
      constValueIndex <- getWord16be
      return $! AnnotationElementConstShortValue (ConstantPoolIndex (toInteger constValueIndex))
    0x5A -> do
      constValueIndex <- getWord16be
      return $! AnnotationElementConstBooleanValue (ConstantPoolIndex (toInteger constValueIndex))
    0x73 -> do
      constValueIndex <- getWord16be
      return $! AnnotationElementConstStringValue (ConstantPoolIndex (toInteger constValueIndex))
    0x65 -> do
      typeNameIndex <- getWord16be
      constNameIndex <- getWord16be
      return $! AnnotationElementEnumConstValue (ConstantPoolIndex (toInteger typeNameIndex)) (ConstantPoolIndex (toInteger constNameIndex))
    0x63 -> do
      classInfoIndex <- getWord16be
      return $! AnnotationElementClassValue (ConstantPoolIndex (toInteger classInfoIndex))
    0x40 -> do
      annotationValue <- deserializeAnnotation
      return $! AnnotationElementAnnotationValue annotationValue
    0x5B -> do
      numValues <- getWord16be
      values <- replicateM (fromIntegral numValues) deserializeAnnotationElementValue
      return $! AnnotationElementArrayValue values
    _ -> fail ("Unknown annotation element value type " ++ (show tag))
