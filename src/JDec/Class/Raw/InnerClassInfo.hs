module JDec.Class.Raw.InnerClassInfo (
InnerClassInfo(InnerClassInfo),
innerClassInfoIndex,
outerClassInfoIndex,
innerNameIndex,
innerClassAccessFlags
) where

import JDec.Class.Raw.ConstantPoolIndex (ConstantPoolIndex)
import JDec.Class.Raw.InnerClassModifier (InnerClassModifier)

import Data.Set

-- | Every ClassConstantPoolEntry in the constant pool table which represents a class or interface C that is not a package member must have exactly one corresponding InnerClassInfo entry in the innerClasses list of InnerClassesAttribute. If a class has members that are classes or interfaces, its constant pool table (and hence its InnerClassesAttribute) must refer to each such member, even if that member is not otherwise mentioned by the class. These rules imply that a nested class or interface member will have InnerClassesAttribute information for each enclosing class and for each immediate member.
data InnerClassInfo = InnerClassInfo {
  innerClassInfoIndex :: ConstantPoolIndex, -- ^ The value must be a valid index into the constant pool table. The constant pool entry at that index must be a ClassConstantPoolEntry representing C. The remaining items in the InnerClassInfo give information about C.
  outerClassInfoIndex :: ConstantPoolIndex, -- ^ If C is not a member of a class or an interface (that is, if C is a top-level class or interface or a local class or an anonymous class), the value must be zero. Otherwise, the value must be a valid index into the constant pool table, and the entry at that index must be a ClassConstantPoolEntry representing the class or interface of which C is a member.
  innerNameIndex :: ConstantPoolIndex, -- ^ If C is anonymous, the value must be zero. Otherwise, the value must be a valid index into the constant pool table, and the entry at that index must be a UTF8ConstantPoolEntry structure that represents the original simple name of C, as given in the source code from which this class file was compiled.
  innerClassAccessFlags :: Set InnerClassModifier -- ^ The value is a set of flags used to denote access permissions to and properties of class or interface C as declared in the source code from which this class file was compiled. It is used by compilers to recover the original information when source code is not available.
} deriving Show
