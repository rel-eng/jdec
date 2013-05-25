module JDec.Class.Raw.LocalVariableTypeInfo (
LocalVariableTypeInfo(LocalVariableTypeInfo),
localVariableTypeStartPC,
localVariableTypeLength,
localVariableTypeNameIndex,
localVariableTypeSignatureIndex,
localVariableTypeIndex
) where

import JDec.Class.Raw.ConstantPoolIndex (ConstantPoolIndex)

-- | It indicates a range of code array offsets within which a local variable has a value. It also indicates the index into the local variable array of the current frame at which that local variable can be found.
data LocalVariableTypeInfo = LocalVariableTypeInfo {
  localVariableTypeStartPC :: Integer, -- ^ The given local variable must have a value at indices into the code array in the interval [localVariableStartPC, localVariableStartPC + localVariableLength), that is, between localVariableStartPC inclusive and localVariableStartPC + localVariableLength exclusive. The value of localVariableStartPC must be a valid index into the code array of the CodeAttribute and must be the index of the opcode of an instruction.
  localVariableTypeLength :: Integer, -- ^ The given local variable must have a value at indices into the code array in the interval [localVariableStartPC, localVariableStartPC + localVariableLength), that is, between localVariableStartPC inclusive and localVariableStartPC + localVariableLength exclusive. The value of localVariableStartPC + localVariableLength must either be a valid index into the code array of the CodeAttribute and be the index of the opcode of an instruction, or it must be the first index beyond the end of that code array.
  localVariableTypeNameIndex :: ConstantPoolIndex, -- ^ The value must be a valid index into the constant pool table. The constant pool entry at that index must contain a UTF8ConstantPoolEntry representing a valid unqualified name denoting a local variable.
  localVariableTypeSignatureIndex :: ConstantPoolIndex, -- ^ The value must be a valid index into the constant pool table. The constant pool entry at that index must contain a UTF8ConstantPoolEntry representing a field type signature encoding the type of a local variable in the source program.
  localVariableTypeIndex :: Integer -- ^ The given local variable must be at index in the local variable array of the current frame. If the local variable at index is of type double or long, it occupies both index and index + 1.
} deriving Show