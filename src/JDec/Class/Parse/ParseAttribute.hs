module JDec.Class.Parse.ParseAttribute (
deserializeAttribute
) where 

import JDec.Class.Raw.Attribute (Attribute(ConstantValueAttribute))
import JDec.Class.Raw.ConstantPoolIndex(ConstantPoolIndex(ConstantPoolIndex))
import JDec.Class.Raw.ConstantPoolEntry (ConstantPoolEntry(UTF8ConstantPoolEntry))

import Data.Binary.Get(Get, getWord16be, getWord32be, getLazyByteString)
import Data.Map as Map (Map, lookup)
import Data.Text (unpack)
import Data.Int(Int64)
import Control.Monad(when, void)

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
            "ConstantValue" -> do
              if attributeLength >= 2 then parseConstantValue attributeLength else skipAttribute attributeLength
            _ -> skipAttribute attributeLength
        _ -> skipAttribute attributeLength
    Nothing -> do
      _ <- getLazyByteString attributeLength
      return $! Nothing

-- | Parse constant value attribute-specific data
parseConstantValue :: Int64 -- ^ Attribute length
  -> Get (Maybe Attribute) -- ^ Attribute, if any
parseConstantValue attributeLength = do
 constantIndex <- getWord16be
 when (attributeLength > 2) (void (getLazyByteString (attributeLength - 2)))
 return $! Just (ConstantValueAttribute (ConstantPoolIndex (toInteger constantIndex)))

-- | Skip attribute
skipAttribute :: Int64 -- ^ Attribute length
  -> Get (Maybe Attribute) -- ^ Nothing
skipAttribute attributeLength = do
  _ <- getLazyByteString attributeLength
  return $! Nothing