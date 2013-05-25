module JDec.Class.Parse.ParseMethod (
deserializeMethod
) where 

import JDec.Class.Raw.Method (Method(Method))
import JDec.Class.Raw.ConstantPoolIndex(ConstantPoolIndex(ConstantPoolIndex))
import JDec.Class.Raw.MethodModifier (MethodModifier(PublicMethodModifier, PrivateMethodModifier, ProtectedMethodModifier, StaticMethodModifier, FinalMethodModifier, SynchronizedMethodModifier, BridgeMethodModifier, VarArgsMethodModifier, NativeMethodModifier, AbstractMethodModifier, StrictMethodModifier, SyntheticMethodModifier))
import JDec.Class.Parse.ParseAttribute(deserializeAttribute)
import JDec.Class.Raw.ConstantPoolEntry (ConstantPoolEntry)

import Data.Binary.Get(Get, getWord16be)
import Data.Set as Set (Set, empty, insert)
import Data.Word(Word16)
import Data.Bits ((.&.))
import Control.Monad(replicateM)
import Data.Map as Map (Map)
import Data.Maybe(catMaybes)

-- | Deserialize method
deserializeMethod :: Map ConstantPoolIndex ConstantPoolEntry -- ^ Constant pool
  -> Get (Method) -- ^ Method
deserializeMethod constantPool = do
  accessFlagsWord <- getWord16be
  nameIndex <- getWord16be
  descriptorIndex <- getWord16be
  attributesCount <- getWord16be
  attributes <- replicateM (fromIntegral attributesCount) (deserializeAttribute constantPool)
  return $! Method (deserializeAccessFlags accessFlagsWord) (ConstantPoolIndex (toInteger nameIndex)) (ConstantPoolIndex (toInteger descriptorIndex)) (catMaybes attributes)

-- | Deserialize field access flags
deserializeAccessFlags :: Word16 -- ^ Word
  -> Set MethodModifier -- ^ Flags set
deserializeAccessFlags word = foldl (\s (x,y) -> if ((word .&. y) /= (0x0000 :: Word16)) then Set.insert x s else s) (Set.empty) [(PublicMethodModifier, 0x0001 :: Word16), (PrivateMethodModifier, 0x0002 :: Word16), (ProtectedMethodModifier, 0x0004 :: Word16), (StaticMethodModifier, 0x0008 :: Word16), (FinalMethodModifier, 0x0010 :: Word16), (SynchronizedMethodModifier, 0x0020 :: Word16), (BridgeMethodModifier, 0x0040 :: Word16), (VarArgsMethodModifier, 0x0080 :: Word16), (NativeMethodModifier, 0x0100 :: Word16), (AbstractMethodModifier, 0x0400 :: Word16), (StrictMethodModifier, 0x0800 :: Word16), (SyntheticMethodModifier, 0x1000 :: Word16)] 
