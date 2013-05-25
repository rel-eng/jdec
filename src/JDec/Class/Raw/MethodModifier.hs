module JDec.Class.Raw.MethodModifier (
MethodModifier(
PublicMethodModifier,
PrivateMethodModifier,
ProtectedMethodModifier,
StaticMethodModifier,
FinalMethodModifier,
SynchronizedMethodModifier,
BridgeMethodModifier,
VarArgsMethodModifier,
NativeMethodModifier,
AbstractMethodModifier,
StrictMethodModifier,
SyntheticMethodModifier)
) where

-- | These flags are used to denote access permission to and properties of the method. Methods of classes may set any of the flags. However, a specific method of a class may have at most one of its PrivateMethodModifier, ProtectedMethodModifier and PublicMethodModifier flags set. If a specific method has its AbstractMethodModifier flag set, it must not have any of its FinalMethodModifier, NativeMethodModifier, PrivateMethodModifier, StaticMethodModifier, StrictMethodModifier or SynchronizedMethodModifier flags set. All interface methods must have their AbstractMethodModifier and PublicMethodModifier flags set; they may have their VarArgsMethodModifier, BridgeMethodModifier and SyntheticMethodModifier flags set and must not have any of the other flags set. A specific instance initialization method may have at most one of its PrivateMethodModifier, ProtectedMethodModifier, and PublicMethodModifier flags set, and may also have its StrictMethodModifier, VarArgsMethodModifier and SyntheticMethodModifier flags set, but must not have any of the other flags set. Class and interface initialization methods are called implicitly by the virtual machine. The value of their flags is ignored except for the setting of the StrictMethodModifier flag.
data MethodModifier = PublicMethodModifier -- ^ Declared public; may be accessed from outside its package.
  | PrivateMethodModifier -- ^ Declared private; accessible only within the defining class.
  | ProtectedMethodModifier -- ^ Declared protected; may be accessed within subclasses.
  | StaticMethodModifier -- ^ Declared static.
  | FinalMethodModifier -- ^ Declared final; must not be overridden.
  | SynchronizedMethodModifier -- ^ Declared synchronized; invocation is wrapped by a monitor use.
  | BridgeMethodModifier -- ^ A bridge method, generated by the compiler.
  | VarArgsMethodModifier -- ^ Declared with variable number of arguments.
  | NativeMethodModifier -- ^ Declared native.
  | AbstractMethodModifier -- ^ Declared abstract; no implementation is provided.
  | StrictMethodModifier -- ^ Declared strictfp; floating-point mode is FP-strict.
  | SyntheticMethodModifier -- ^ Declared synthetic; not present in the source code.
  deriving Show