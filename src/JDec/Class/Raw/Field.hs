module JDec.Class.Raw.Field (
Field(Field),
fieldAccessFlags,
fieldNameIndex,
fieldDescriptorIndex,
fieldAttributes
) where

import JDec.Class.Raw.FieldModifier(FieldModifier)
import JDec.Class.Raw.Attribute(Attribute)
import JDec.Class.Raw.ConstantPoolIndex (ConstantPoolIndex)

import Data.Set

-- | Field decription. No two fields in one class file may have the same name and descriptor.
data Field = Field {
  fieldAccessFlags :: Set FieldModifier, -- ^ The value is a set of flags used to denote access permission to and properties of the field.
  fieldNameIndex :: ConstantPoolIndex, -- ^ The value must be a valid index into the constant pool table. The constant pool entry at that index must be a UTF8ConstantPoolEntry which must represent a valid unqualified name denoting a field.
  fieldDescriptorIndex :: ConstantPoolIndex, -- ^ The value must be a valid index into the constant pool table. The constant pool entry at that index must be a UTF8ConstantPoolEntry that must represent a valid field descriptor.
  fieldAttributes :: [Attribute] -- ^ Attributes of this field. ConstantValueAttribute, SyntheticAttribute, SignatureAttribute, DeprecatedAttribute, RuntimeVisibleAnnotationsAttribute, RuntimeInvisibleAnnotationsAttribute attributes may appear here.
} deriving Show
