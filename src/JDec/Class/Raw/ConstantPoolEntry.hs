module JDec.Class.Raw.ConstantPoolEntry (
ConstantPoolEntry(
  ClassConstantPoolEntry,
  FieldRefConstantPoolEntry,
  MethodRefConstantPoolEntry,
  InterfaceMethodRefConstantPoolEntry,
  StringConstantPoolEntry,
  IntegerConstantPoolEntry,
  FloatConstantPoolEntry,
  LongConstantPoolEntry,
  DoubleConstantPoolEntry,
  NameAndTypeConstantPoolEntry,
  UTF8ConstantPoolEntry,
  MethodHandleConstantPoolEntry,
  MethodTypeConstantPoolEntry,
  InvokeDynamicConstantPoolEntry),
classNameIndex,
fieldClassIndex,
fieldNameAndTypeIndex,
methodClassIndex,
methodNameAndTypeIndex,
interfaceMethodClassIndex,
interfaceMethodNameAndTypeIndex,
constantStringIndex,
constantIntValue,
constantFloatValue,
constantLongValue,
constantDoubleValue,
nameAndTypeNameIndex,
nameAndTypeDescriptorIndex,
constantStringValue,
methodHandleReferenceKind,
methodHandleReferenceIndex,
methodTypeDescriptorIndex,
invokeDynamicBootstrapMethodAttrIndex,
invokeDynamicNameAndTypeIndex
) where

import JDec.Class.Raw.MethodHandleKind(MethodHandleKind)
import JDec.Class.Raw.ConstantPoolIndex (ConstantPoolIndex)

import Data.Text

-- | Structures representing various string constants, class and interface names, field names, and other constants that are referred to within the ClassFile structure and its substructures.
data ConstantPoolEntry = ClassConstantPoolEntry { -- ^ It is used to represent a class or an interface.
    classNameIndex :: ConstantPoolIndex -- ^ The value must be a valid index into the constant pool table. The constant pool entry at that index must be a UTF8ConstantPoolEntry representing a valid binary class or interface name encoded in internal form.
  }
  | FieldRefConstantPoolEntry { -- ^ It is used to represent a field.
    fieldClassIndex :: ConstantPoolIndex, -- ^ The value must be a valid index into the constant pool table. The constant pool entry at that index must be a ClassConstantPoolEntry representing a class or interface type that has the field as a member.
    fieldNameAndTypeIndex :: ConstantPoolIndex -- ^ The value must be a valid index into the constant pool table. The constant pool entry at that index must be a NameAndTypeConstantPoolEntry. This constant pool entry indicates the name and descriptor of the field. The indicated descriptor must be a field descriptor.
  }
  | MethodRefConstantPoolEntry { -- ^ It is used to represent a method.
    methodClassIndex :: ConstantPoolIndex, -- ^ The value must be a valid index into the constant pool table. The constant pool entry at that index must be a ClassConstantPoolEntry representing a class type that has the method as a member.
    methodNameAndTypeIndex :: ConstantPoolIndex -- ^ The value must be a valid index into the constant pool table. The constant pool entry at that index must be a NameAndTypeConstantPoolEntry. This constant pool entry indicates the name and descriptor of the method. The indicated descriptor must be a method descriptor.
  }
  | InterfaceMethodRefConstantPoolEntry { -- ^ It is used to represent an interface method.
    interfaceMethodClassIndex :: ConstantPoolIndex, -- ^ The value must be a valid index into the constant pool table. The constant pool entry at that index must be a ClassConstantPoolEntry representing an interface type that has the method as a member.
    interfaceMethodNameAndTypeIndex :: ConstantPoolIndex  -- ^ The value must be a valid index into the constant pool table. The constant pool entry at that index must be a NameAndTypeConstantPoolEntry. This constant pool entry indicates the name and descriptor of the method. The indicated descriptor must be a method descriptor.
  }
  | StringConstantPoolEntry { -- ^ It is used to represent constant objects of the type String.
    constantStringIndex :: ConstantPoolIndex -- ^ The value must be a valid index into the constant pool table. The constant pool entry at that index must be a UTF8ConstantPoolEntry representing the sequence of Unicode code points to which the String object is to be initialized.
  }
  | IntegerConstantPoolEntry { -- ^ It represent 4-byte numeric integer constant.
    constantIntValue :: Integer -- ^ The value of the integer constant.
  }
  | FloatConstantPoolEntry { -- ^ It represent 4-byte numeric floating point constant.
    constantFloatValue :: Float -- ^ The value of the floating point constant.
  }
  | LongConstantPoolEntry { -- ^ It represent 8-byte numeric integer constant. It takes up two entries in the constant pool table. If a LongConstantPoolEntry is the item in the constant pool table at index n, then the next usable item in the pool is located at index n+2. The constant pool index n+1 must be valid but is considered unusable.
    constantLongValue :: Integer -- ^ The value of the integer constant.
  }
  | DoubleConstantPoolEntry { -- ^ It represent 8-byte numeric floating point constant. It takes up two entries in the constant pool table. If a DoubleConstantPoolEntry is the item in the constant pool table at index n, then the next usable item in the pool is located at index n+2. The constant pool index n+1 must be valid but is considered unusable.
    constantDoubleValue :: Double -- ^ The value of the floating point constant.
  }
  | NameAndTypeConstantPoolEntry { -- ^ It is used to represent a field or method, without indicating which class or interface type it belongs to.
    nameAndTypeNameIndex :: ConstantPoolIndex, -- The value must be a valid index into the constant pool table. The constant pool entry at that index must be a UTF8ConstantPoolEntry representing either the special method name <init> or a valid unqualified name denoting a field or method.
    nameAndTypeDescriptorIndex :: ConstantPoolIndex -- ^ The value must be a valid index into the constant pool table. The constant pool entry at that index must be a UTF8ConstantPoolEntry representing a valid field descriptor or method descriptor.
  }
  | UTF8ConstantPoolEntry { -- ^ It is used to represent constant string values.
    constantStringValue :: Text -- ^ The value of the string constant.
  }
  | MethodHandleConstantPoolEntry { -- ^ It is used to represent a method handle.
    methodHandleReferenceKind :: MethodHandleKind, -- ^ The value denotes the kind of this method handle, which characterizes its bytecode behavior.
    methodHandleReferenceIndex :: ConstantPoolIndex -- ^ The value must be a valid index into the constant pool table. If the value is GetFieldMethodHandleKind, GetStaticMethodHandleKind, PutFieldMethodHandleKind, or PutStaticMethodHandleKind, then the constant pool entry at that index must be a FieldRefConstantPoolEntry representing a field for which a method handle is to be created. If the value is InvokeVirtualMethodHandleKind, InvokeStaticMethodHandleKind, InvokeSpecialMethodHandleKind, or NewInvokeSpecialMethodHandleKind, then the constant pool entry at that index must be a MethodRefConstantPoolEntry representing a class's method or constructor for which a method handle is to be created. If the value is InvokeInterfaceMethodHandleKind, then the constant pool entry at that index must be a InterfaceMethodRefConstantPoolEntry representing an interface's method for which a method handle is to be created. If the value is InvokeVirtualMethodHandleKind, InvokeStaticMethodHandleKind, InvokeSpecialMethodHandleKind, or InvokeInterfaceMethodHandleKind, the name of the method represented by a MethodRefConstantPoolEntry must not be <init> or <clinit>. If the value is NewInvokeSpecialMethodHandleKind, the name of the method represented by a MethodRefConstantPoolEntry must be <init>.
  }
  | MethodTypeConstantPoolEntry { -- ^ It is used to represent a method type.
    methodTypeDescriptorIndex :: ConstantPoolIndex -- ^ The value must be a valid index into the constant pool table. The constant pool entry at that index must be a UTF8ConstantPoolEntry representing a method descriptor.
  }
  | InvokeDynamicConstantPoolEntry { -- ^ It is used by an invokedynamic instruction to specify a bootstrap method, the dynamic invocation name, the argument and return types of the call, and optionally, a sequence of additional constants called static arguments to the bootstrap method.
    invokeDynamicBootstrapMethodAttrIndex :: Integer, -- ^ The value must be a valid index into the bootstrap methods array of the bootstrap method table of the class file.
    invokeDynamicNameAndTypeIndex :: ConstantPoolIndex -- ^ The value must be a valid index into the constant pool table. The constant pool entry at that index must be a NameAndTypeConstantPoolEntry structure representing a method name and method descriptor.
  } deriving Show
