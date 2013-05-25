module JDec.Class.Parse.ParseField (
deserializeField
) where 

import JDec.Class.Raw.Field (Field(Field))
import JDec.Class.Raw.ConstantPoolIndex(ConstantPoolIndex(ConstantPoolIndex))
import JDec.Class.Raw.FieldModifier (FieldModifier(PublicFieldModifier, PrivateFieldModifier, ProtectedFieldModifier, StaticFieldModifier, FinalFieldModifier, VolatileFieldModifier, TransientFieldModifier, SyntheticFieldModifier, EnumFieldModifier))
import JDec.Class.Parse.ParseAttribute(deserializeAttribute)
import JDec.Class.Raw.ConstantPoolEntry (ConstantPoolEntry)

import Data.Binary.Get(Get, getWord16be)
import Data.Set as Set (Set, empty, insert)
import Data.Word(Word16)
import Data.Bits ((.&.))
import Control.Monad(replicateM)
import Data.Map as Map (Map)
import Data.Maybe(catMaybes)

-- | Deserialize field
deserializeField :: Map ConstantPoolIndex ConstantPoolEntry -- ^ Constant pool
  -> Get (Field) -- ^ Field
deserializeField constantPool = do
  accessFlagsWord <- getWord16be
  nameIndex <- getWord16be
  descriptorIndex <- getWord16be
  attributesCount <- getWord16be
  attributes <- replicateM (fromIntegral attributesCount) (deserializeAttribute constantPool)
  return $! Field (deserializeAccessFlags accessFlagsWord) (ConstantPoolIndex (toInteger nameIndex)) (ConstantPoolIndex (toInteger descriptorIndex)) (catMaybes attributes)

-- | Deserialize field access flags
deserializeAccessFlags :: Word16 -- ^ Word
  -> Set FieldModifier -- ^ Flags set
deserializeAccessFlags word = foldl (\s (x,y) -> if ((word .&. y) /= (0x0000 :: Word16)) then Set.insert x s else s) (Set.empty) [(PublicFieldModifier, 0x0001 :: Word16), (PrivateFieldModifier, 0x0002 :: Word16), (ProtectedFieldModifier, 0x0004 :: Word16), (StaticFieldModifier, 0x0008 :: Word16), (FinalFieldModifier, 0x0010 :: Word16), (VolatileFieldModifier, 0x0040 :: Word16), (TransientFieldModifier, 0x0080 :: Word16), (SyntheticFieldModifier, 0x1000 :: Word16), (EnumFieldModifier, 0x4000 :: Word16)]