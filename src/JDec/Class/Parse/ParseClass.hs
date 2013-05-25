module JDec.Class.Parse.ParseClass (
deserializeClass
) where

import JDec.Class.Raw.Class (Class(Class))
import JDec.Class.Raw.ClassVersion (ClassVersion(ClassVersion))
import JDec.Class.Raw.ConstantPoolIndex(ConstantPoolIndex(ConstantPoolIndex))
import JDec.Class.Parse.ParseConstantPoolEntry(deserializeConstantPoolEntry)
import JDec.Class.Raw.ConstantPoolEntry(ConstantPoolEntry, ConstantPoolEntry(LongConstantPoolEntry, DoubleConstantPoolEntry))
import JDec.Class.Raw.ClassModifier(ClassModifier(PublicClassModifier, FinalClassModifier, SuperClassModifier, InterfaceClassModifier, AbstractClassModifier, SyntheticClassModifier, AnnotationClassModifier, EnumClassModifier))
import JDec.Class.Parse.ParseField(deserializeField)

import Data.Binary.Get(Get, getWord32be, getWord16be)
import Data.Map as Map (Map, empty, insert)
import Data.Set as Set (Set, empty, insert)
import Data.Word(Word16)
import Data.Bits ((.&.))
import Control.Monad(replicateM)

-- | Deserialize class
deserializeClass :: Get (Class) -- ^ Class
deserializeClass = do
  magic <- getWord32be
  if magic == 0xCAFEBABE
     then do
       minorVersion <- getWord16be
       majorVersion <- getWord16be
       constantPoolCount <- getWord16be
       constantPoolEntries <- readConstantPool Map.empty ((toInteger constantPoolCount)-1) 1
       accessFlagsWord <- getWord16be
       thisClassIndex <- getWord16be
       superClassIndex <- getWord16be
       interfacesCount <- getWord16be
       interfaceIndexes <- replicateM (fromIntegral interfacesCount) (fmap (ConstantPoolIndex . toInteger) getWord16be)
       fieldsCount <- getWord16be
       fields <- replicateM (fromIntegral fieldsCount) (deserializeField constantPoolEntries)
       return $! Class (ClassVersion (toInteger minorVersion) (toInteger majorVersion)) constantPoolEntries (deserializeAccessFlags accessFlagsWord) (ConstantPoolIndex (toInteger thisClassIndex)) (ConstantPoolIndex (toInteger superClassIndex)) interfaceIndexes fields [] []
     else fail "Invalid file header"

-- ^ Read all remaining constant pool entries
readConstantPool :: Map ConstantPoolIndex ConstantPoolEntry -- ^ Map of constant pool entries read so far
  -> Integer -- ^ Entries left to read
  -> Integer -- ^ Next entry index
  -> Get (Map ConstantPoolIndex ConstantPoolEntry) -- ^ Map of constant pool entries
readConstantPool entries entriesLeftToRead entryIndex
  | entriesLeftToRead == 0 = return $! entries
  | otherwise = do
    entry <- deserializeConstantPoolEntry
    case entry of
      LongConstantPoolEntry _ -> readConstantPool (Map.insert (ConstantPoolIndex entryIndex) entry entries) (entriesLeftToRead - 2) (entryIndex + 2)
      DoubleConstantPoolEntry _ -> readConstantPool (Map.insert (ConstantPoolIndex entryIndex) entry entries) (entriesLeftToRead - 2) (entryIndex + 2)
      _ -> readConstantPool (Map.insert (ConstantPoolIndex entryIndex) entry entries) (entriesLeftToRead - 1) (entryIndex + 1)

-- | Deserialize class access flags
deserializeAccessFlags :: Word16 -- ^ Word
  -> Set ClassModifier -- ^ Flags set
deserializeAccessFlags word = foldl (\s (x,y) -> if ((word .&. y) /= (0x0000 :: Word16)) then Set.insert x s else s) (Set.empty) [(PublicClassModifier, 0x0001 :: Word16), (FinalClassModifier, 0x0010 :: Word16), (SuperClassModifier, 0x0020 :: Word16), (InterfaceClassModifier, 0x0200 :: Word16), (AbstractClassModifier, 0x0400 :: Word16), (SyntheticClassModifier, 0x1000 :: Word16), (AnnotationClassModifier, 0x2000 :: Word16), (EnumClassModifier, 0x4000 :: Word16)]