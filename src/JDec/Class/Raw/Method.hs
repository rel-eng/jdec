module JDec.Class.Raw.Method (
Method(Method),
methodAccessFlags,
methodNameIndex,
methodDescriptorIndex,
methodAttributes
) where

import JDec.Class.Raw.MethodModifier(MethodModifier)
import JDec.Class.Raw.Attribute(Attribute)
import JDec.Class.Raw.ConstantPoolIndex (ConstantPoolIndex)

import Data.Set

-- | Each method, including each instance initialization method and the class or interface initialization method have such a description. No two methods in one class file may have the same name and descriptor.
data Method = Method {
  methodAccessFlags :: Set MethodModifier, -- ^ The value is a set of flags used to denote access permission to and properties of the method.
  methodNameIndex :: ConstantPoolIndex, -- ^ The value must be a valid index into the constant pool table. The constant pool entry at that index must be a UTF8ConstantPoolEntry representing either one of the special method names <init> or <clinit>, or a valid unqualified name denoting a method.
  methodDescriptorIndex :: ConstantPoolIndex, -- ^ The value must be a valid index into the constant pool table. The constant pool entry at that index must be a UTF8ConstantPoolEntry representing a valid method descriptor.
  methodAttributes :: [Attribute] -- ^ Attributes of this method. CodeAttribute, ExceptionsAttribute, SyntheticAttribute, SignatureAttribute, DeprecatedAttribute, RuntimeVisibleAnnotationsAttribute, RuntimeInvisibleAnnotationsAttribute, RuntimeVisibleParameterAnnotationsAttribute, RuntimeInvisibleParameterAnnotationsAttribute, AnnotationDefaultAttribute may appear here
} deriving Show
