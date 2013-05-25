module JDec.Class.Raw.BootstrapMethodInfo (
  BootstrapMethodInfo(BootstrapMethodInfo),
  bootstrapMethodRef,
  bootstrapArguments
) where

import JDec.Class.Raw.ConstantPoolIndex (ConstantPoolIndex)

-- | Contains an index to a MethodHandleConstantPoolEntry which specifies a bootstrap method, and a sequence (perhaps empty) of indexes to static arguments for the bootstrap method.
data BootstrapMethodInfo = BootstrapMethodInfo {
  bootstrapMethodRef :: ConstantPoolIndex, -- ^ The value must be a valid index into the constant pool table. The constant pool entry at that index must be a MethodHandleConstantPoolEntry.
  bootstrapArguments :: [ConstantPoolIndex] -- ^ Each entry must be a valid index into the constant pool table. The constant pool entry at that index must be a StringConstantPoolEntry, ClassConstantPoolEntry, IntegerConstantPoolEntry, LongConstantPoolEntry, FloatConstantPoolEntry, DoubleConstantPoolEntry, MethodHandleConstantPoolEntry, or MethodTypeConstantPoolEntry.
} deriving Show