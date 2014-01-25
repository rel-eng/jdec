module JDec.Class.Typesafe.Class (
Class(Class),
version,
accessFlags,
thisClass,
superClass,
interfaces,
fields,
methods,
innerClasses,
enclosingMethod,
synthetic,
genericSignature,
sourceFile,
sourceDebugExtension,
deprecated,
runtimeVisibleAnnotations,
runtimeInvisibleAnnotations,
bootstrapMethods
) where

import JDec.Class.Raw.ClassModifier (ClassModifier)
import JDec.Class.Raw.ClassVersion (ClassVersion)
import JDec.Class.Typesafe.Field(Field)
import JDec.Class.Typesafe.Method(Method)
import JDec.Class.Typesafe.InnerClassInfo(InnerClassInfo)
import JDec.Class.Typesafe.EnclosingMethodInfo(EnclosingMethodInfo)
import JDec.Class.Typesafe.Annotation(Annotation)
import JDec.Class.Typesafe.BootstrapMethodInfo(BootstrapMethodInfo)

import Data.Set(Set)
import Data.Text(Text)

-- | A class file
data Class = Class {
  version :: ClassVersion, -- ^ Version of the class file.
  accessFlags :: Set ClassModifier, -- ^ The value is a set of flags used to denote access permissions to and properties of the class or interface.
  thisClass :: Text, -- ^ Name of the class or interface defined by this class file.
  superClass :: Maybe Text, -- ^ Name of the direct superclass of the class defined by this class file, None if this class is Object, name of the Object class if it is interface.
  interfaces :: [Text], -- ^ Each entry represents an interface that is a direct superinterface of this class or interface type, in the left-to-right order given in the source for the type.
  fields :: [Field], -- ^ Each value in the fields table must be a field giving a complete description of a field in this class or interface. The fields table includes only those fields that are declared by this class or interface. It does not include items representing fields that are inherited from superclasses or superinterfaces.
  methods :: [Method], -- ^ Each value in the methods table must be a method giving a complete description of a method in this class or interface. If neither of the NativeMethodModifier and AbstractMethodModifier flags are set, the virtual machine instructions implementing the method are also supplied. These methods represent all methods declared by this class or interface type, including instance methods, class methods, instance initialization methods, and any class or interface initialization method. The methods table does not include items representing methods that are inherited from superclasses or superinterfaces.
  innerClasses :: [InnerClassInfo], -- ^ Inner class info entries
  enclosingMethod :: Maybe EnclosingMethodInfo, -- ^ Enclosing method info if this class is local or anonymous
  synthetic :: Bool, -- ^ Is this class synthetic?
  genericSignature :: Maybe Text, -- ^ Generic class signature, if present
  sourceFile :: Maybe Text, -- ^ Class source file name, if present
  sourceDebugExtension :: Maybe Text, -- ^ Source debug extension data, if present
  deprecated :: Bool, -- ^ Is this class deprecated?
  runtimeVisibleAnnotations :: [Annotation], -- ^ Runtime visible annotations
  runtimeInvisibleAnnotations :: [Annotation], -- ^ Runtime invisible annotations
  bootstrapMethods :: [BootstrapMethodInfo] -- ^ Bootstrap methods
} deriving Show
