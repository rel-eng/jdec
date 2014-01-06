module JDec.Class.Raw.Attribute (
Attribute(
  ConstantValueAttribute,
  CodeAttribute,
  StackMapTableAttribute,
  ExceptionsAttribute,
  InnerClassesAttribute,
  EnclosingMethodAttribute,
  SyntheticAttribute,
  SignatureAttribute,
  SourceFileAttribute,
  SourceDebugExtensionAttribute,
  LineNumberTableAttribute,
  LocalVariableTableAttribute,
  LocalVariableTypeTableAttribute,
  DeprecatedAttribute,
  RuntimeVisibleAnnotationsAttribute,
  RuntimeInvisibleAnnotationsAttribute,
  RuntimeVisibleParameterAnnotationsAttribute,
  RuntimeInvisibleParameterAnnotationsAttribute,
  AnnotationDefaultAttribute,
  BootstrapMethodsAttribute),
constantValueIndex,
exceptionIndexTable,
enclosingMethodClassIndex,
enclosingMethodIndex,
signatureIndex,
sourceFileIndex,
debugExtension,
innerClasses,
lineNumbers,
localVariables,
localVariablesTypes,
runtimeVisibleAnnotations,
runtimeInvisibleAnnotations,
runtimeVisibleParameterAnnotations,
runtimeInvisibleParameterAnnotations,
annotationDefaultValue,
stackMapFrames,
bootstrapMethods,
maxStack,
maxLocals,
instructions,
exceptionHandlers,
codeAttributes
) where

import JDec.Class.Raw.ConstantPoolIndex (ConstantPoolIndex)
import JDec.Class.Raw.InnerClassInfo (InnerClassInfo)
import JDec.Class.Raw.LineNumberInfo(LineNumberInfo)
import JDec.Class.Raw.LocalVariableInfo(LocalVariableInfo)
import JDec.Class.Raw.LocalVariableTypeInfo(LocalVariableTypeInfo)
import JDec.Class.Raw.Annotation(Annotation, AnnotationElementValue)
import JDec.Class.Raw.StackMapFrame(StackMapFrame)
import JDec.Class.Raw.BootstrapMethodInfo(BootstrapMethodInfo)
import JDec.Class.Raw.ExceptionHandlerInfo(ExceptionHandlerInfo)
import JDec.Bytecode.Raw.Instruction(Instruction)

import Data.Text
import Data.Map

-- | An attribute.
data Attribute = ConstantValueAttribute { -- ^ It is an attribute in the attributes table of a field. It represents the value of a constant field. There can be no more than one such attribute in the attributes table of a given field. If the field is static then the constant field is assigned the value referenced by this attribute as part of the initialization of the class or interface declaring the constant field. This occurs prior to the invocation of the class or interface initialization method of that class or interface. If a non-static field has such an attribute, then that attribute must silently be ignored.
    constantValueIndex :: ConstantPoolIndex -- ^ The value must be a valid index into the constant pool table. The constant pool entry at that index gives the constant value represented by this attribute. The constant pool entry must be of a type appropriate to the field.
  }
  | CodeAttribute { -- ^ It is an attribute in the attributes table of a method. It contains the virtual machine instructions and auxiliary information for a single method, instance initialization method, or class or interface initialization method. If the method is either native or abstract, it must not have such an attribute. Otherwise, it must have exactly one such attribute.
    maxStack :: Integer, -- ^ The value gives the maximum depth of the operand stack of this method at any point during execution of the method.
    maxLocals :: Integer, -- ^ The value gives the number of local variables in the local variable array allocated upon invocation of this method, including the local variables used to pass parameters to the method on its invocation. The greatest local variable index for a value of type long or double is maxLocals - 2. The greatest local variable index for a value of any other type is maxLocals - 1.
    instructions :: Map Integer Instruction, -- ^ Instructions
    exceptionHandlers :: [ExceptionHandlerInfo], -- ^ Each entry describes one exception handler in the code array. The order of the handlers is significant.
    codeAttributes :: [Attribute] -- ^ The only attributes that appear in the attributes table of a Code attribute are the LineNumberTable, LocalVariableTable, LocalVariableTypeTable, and StackMapTable attributes.
  }
  | StackMapTableAttribute { -- ^ It is an attribute in the attributes table of a CodeAttribute. This attribute is used during the process of verification by typechecking. A method's CodeAttribute may have at most one such attribute.
    stackMapFrames :: [StackMapFrame] -- ^ The method's stack map frames
  }
  | ExceptionsAttribute { -- ^ It is an attribute in the attributes table of a method. It indicates which checked exceptions a method may throw. There may be at most one such attribute in each method.
    exceptionIndexTable :: [ConstantPoolIndex] -- ^ Each value in the exception index table must be a valid index into the constant pool table. The constant pool entry referenced by each table item must be a ClassConstantPoolEntry structure representing a class type that this method is declared to throw.
  }
  | InnerClassesAttribute { -- ^ It is an attribute in the attributes table of a class. If the constant pool of a class or interface C contains a ClassConstantPoolEntry entry which represents a class or interface that is not a member of a package, then C must have exactly one InnerClassesAttribute in its attributes table.
    innerClasses :: [InnerClassInfo] -- ^ Every ClassConstantPoolEntry in the constant pool table which represents a class or interface C that is not a package member must have exactly one corresponding InnerClassInfo entry in the innerClasses list of InnerClassesAttribute. If a class has members that are classes or interfaces, its constant pool table (and hence its InnerClassesAttribute) must refer to each such member, even if that member is not otherwise mentioned by the class. These rules imply that a nested class or interface member will have InnerClassesAttribute information for each enclosing class and for each immediate member.
  }
  | EnclosingMethodAttribute { -- ^ It is an optional attribute in the attributes table of a class. A class must have an such an attribute if and only if it is a local class or an anonymous class. A class may have no more than one such attribute.
    enclosingMethodClassIndex :: ConstantPoolIndex, -- ^ The value must be a valid index into the constant pool table. The constant pool entry at that index must be a ClassConstantPoolEntry representing the innermost class that encloses the declaration of the current class.
    enclosingMethodIndex :: ConstantPoolIndex -- ^ If the current class is not immediately enclosed by a method or constructor, then the value must be zero. Otherwise, the value must be a valid index into the constant pool table. The constant pool entry at that index must be a NameAndTypeConstantPoolEntry representing the name and type of a method in the class referenced by the classIndex.
  }
  | SyntheticAttribute -- ^ It is an attribute in the attributes table of a class, field or method. A class member that does not appear in the source code must be marked using a Synthetic attribute, or else it must have its synthetic flag set. The only exceptions to this requirement are compiler-generated methods which are not considered implementation artifacts, namely the instance initialization method representing a default constructor, the class initialization method, and the Enum.values() and Enum.valueOf() methods.
  | SignatureAttribute { -- ^ It is an optional attribute in the attributes table of a class, field or method. This attribute records generic signature information for any class, interface, constructor or member whose generic signature would include references to type variables or parameterized types.
    signatureIndex :: ConstantPoolIndex -- ^ The value must be a valid index into the constant pool table. The constant pool entry at that index must be a UTF8ConstantPoolEntry representing either a class signature, if this signature attribute is an attribute of a class, a method type signature, if this signature is an attribute of a method, or a field type signature otherwise.
  }
  | SourceFileAttribute { -- ^ It is an optional attribute in the attributes table of a class. There can be no more than one such attribute in the attributes table of a given class.
    sourceFileIndex :: ConstantPoolIndex -- ^ The value must be a valid index into the constant pool table. The constant pool entry at that index must be a UTF8ConstantPoolEntry representing a string.
  }
  | SourceDebugExtensionAttribute { -- ^ It is an optional attribute in the attributes table of a class. There can be no more than one such attribute in the attributes table of a given class.
    debugExtension :: Text -- ^ It holds extended debugging information which has no semantic effect on the virtual machine.
  }
  | LineNumberTableAttribute { -- ^ It is an optional attribute in the attributes table of a CodeAttribute. It may be used by debuggers to determine which part of the virtual machine code array corresponds to a given line number in the original source file.
    lineNumbers :: [LineNumberInfo] -- ^ Each LineNumberInfo indicates that the line number in the original source file changes at a given point in the code array.
  }
  | LocalVariableTableAttribute { -- ^ It is an optional attribute in the attributes table of a CodeAttribute. It may be used by debuggers to determine the value of a given local variable during the execution of a method.
    localVariables :: [LocalVariableInfo] -- ^ Each LocalVariableInfo indicates a range of code array offsets within which a local variable has a value. It also indicates the index into the local variable array of the current frame at which that local variable can be found.
  }
  | LocalVariableTypeTableAttribute { -- ^ It is an optional attribute in the attributes table of a CodeAttribute. It may be used by debuggers to determine the value of a given local variable during the execution of a method. The LocalVariableTypeTableAttribute differs from the LocalVariableTableAttribute in that it provides signature information rather than descriptor information.
    localVariablesTypes :: [LocalVariableTypeInfo] -- ^ Each LocalVariableTypeInfo indicates a range of code array offsets within which a local variable has a value. It also indicates the index into the local variable array of the current frame at which that local variable can be found.
  }
  | DeprecatedAttribute -- ^ It is an optional attribute in the attributes table of a class, field or method. A class, interface, method, or field may be marked using a DeprecatedAttribute to indicate that the class, interface, method, or field has been superseded.
  | RuntimeVisibleAnnotationsAttribute { -- ^ It is an attribute in the attributes table of a class, field or method. It records runtime-visible annotations on the corresponding class, field, or method.
    runtimeVisibleAnnotations :: [Annotation] -- ^ Each value represents a single runtime-visible annotation on a program element.
  }
  | RuntimeInvisibleAnnotationsAttribute { -- ^ This attribute is similar to the RuntimeVisibleAnnotationsAttribute, except that the annotations represented by this attribute must not be made available for return by reflective APIs, unless the virtual machine has been instructed to retain these annotations via some implementation-specific mechanism such as a command line flag. In the absence of such instructions, the virtual machine ignores this attribute.
    runtimeInvisibleAnnotations :: [Annotation] -- ^ Each value represents a single runtime-invisible annotation on a program element.
  }
  | RuntimeVisibleParameterAnnotationsAttribute { -- ^ It is an attribute in the attributes table of the method. It records runtime-visible Java programming language annotations on the parameters of the corresponding method.
    runtimeVisibleParameterAnnotations :: [[Annotation]] -- ^ Each value represents all of the runtime-visible annotations on a single parameter. The sequence of values corresponds to the sequence of parameters in the method descriptor.
  }
  | RuntimeInvisibleParameterAnnotationsAttribute { -- ^ This attribute is similar to the RuntimeVisibleParameterAnnotationsAttribute, except that the annotations represented by this attribute must not be made available for return by reflective APIs, unless the the virtual machine has specifically been instructed to retain these annotations via some implementation-specific mechanism such as a command line flag. In the absence of such instructions, the virtual machine ignores this attribute.
    runtimeInvisibleParameterAnnotations :: [[Annotation]] -- ^ Each value represents all of the runtime-invisible annotations on a single parameter. The sequence of values corresponds to the sequence of parameters in the method descriptor.
  }
  | AnnotationDefaultAttribute { -- ^ It is an attribute in the attributes table of certain methods, namely those representing elements of annotation types. This attribute records the default value for the element represented by the method.
    annotationDefaultValue :: AnnotationElementValue -- ^ It represents the default value of the annotation type element.
  }
  | BootstrapMethodsAttribute { -- ^ This attribute is an attribute in the attributes table of a class. This attribute records bootstrap method specifiers referenced by invokedynamic instructions.
    bootstrapMethods :: [BootstrapMethodInfo] -- ^ Bootstrap methods
  } deriving Show
