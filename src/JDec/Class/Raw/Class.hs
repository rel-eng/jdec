module JDec.Class.Raw.Class (
Class(Class),
classVersion,
classConstantPool,
classAccessFlags,
classThisClass,
classSuperClass,
classInterfaces,
classFields,
classMethods,
classAttributes
) where

import JDec.Class.Raw.ClassVersion (ClassVersion)
import JDec.Class.Raw.Attribute (Attribute)
import JDec.Class.Raw.Field (Field)
import JDec.Class.Raw.ConstantPoolEntry (ConstantPoolEntry)
import JDec.Class.Raw.ClassModifier (ClassModifier)
import JDec.Class.Raw.ConstantPoolIndex (ConstantPoolIndex)
import JDec.Class.Raw.Method (Method)

import Data.Map
import Data.Set

-- | A class file
data Class = Class {
  classVersion :: ClassVersion, -- ^ Version of the class file.
  classConstantPool :: Map ConstantPoolIndex ConstantPoolEntry, -- ^ Constant pool table. The constant pool table is indexed from 1 to constantPoolCount-1.
  classAccessFlags :: Set ClassModifier, -- ^ The value is a set of flags used to denote access permissions to and properties of the class or interface.
  classThisClass :: ConstantPoolIndex, -- ^ The value must be a valid index into the constant pool table. The constant pool entry at that index must be a ClassConstantPoolEntry representing the class or interface defined by this class file.
  classSuperClass :: ConstantPoolIndex, -- ^ For a class, the value either must be zero or must be a valid index into the constant pool table. If the value is nonzero, the constant pool entry at that index must be a ClassConstantPoolEntry structure representing the direct superclass of the class defined by this class file. Neither the direct superclass nor any of its superclasses may have the FinalClassModifier flag set in the accessFlags item of its Class. If the value is zero, then this class file must represent the class Object, the only class or interface without a direct superclass. For an interface, the value must always be a valid index into the constant pool table. The constant pool entry at that index must be a ClassConstantPoolEntry representing the class Object.
  classInterfaces :: [ConstantPoolIndex], -- ^ Each value in the interfaces list must be a valid index into the constant pool table. The constant pool entry at each value of interfaces[i], where 0 <= i < interfacesCount, must be a ClassConstantPoolEntry representing an interface that is a direct superinterface of this class or interface type, in the left-to-right order given in the source for the type.
  classFields :: [Field], -- ^ Each value in the fields table must be a field giving a complete description of a field in this class or interface. The fields table includes only those fields that are declared by this class or interface. It does not include items representing fields that are inherited from superclasses or superinterfaces.
  classMethods :: [Method], -- ^ Each value in the methods table must be a method giving a complete description of a method in this class or interface. If neither of the NativeMethodModifier and AbstractMethodModifier flags are set, the virtual machine instructions implementing the method are also supplied. THese methods represent all methods declared by this class or interface type, including instance methods, class methods, instance initialization methods, and any class or interface initialization method. The methods table does not include items representing methods that are inherited from superclasses or superinterfaces.
  classAttributes :: [Attribute] -- ^ Attributes of this class. InnerClassesAttribute, EnclosingMethodAttribute, SyntheticAttribute, SignatureAttribute, SourceFileAttribute, SourceDebugExtensionAttribute, DeprecatedAttribute, RuntimeVisibleAnnotationsAttribute, RuntimeInvisibleAnnotationsAttribute, BootstrapMethodsAttribute may appear here.
} deriving Show
