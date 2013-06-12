module JDec.Class.Raw.InnerClassModifier (
InnerClassModifier(
PublicInnerClassModifier,
PrivateInnerClassModifier,
ProtectedInnerClassModifier,
StaticInnerClassModifier,
FinalInnerClassModifier,
InterfaceInnerClassModifier,
AbstractInnerClassModifier,
SyntheticInnerClassModifier,
AnnotationInnerClassModifier,
EnumInnerClassModifier)
) where

-- | These flags are used to denote access permissions to and properties of class or interface C as declared in the source code from which this class file was compiled. It is used by compilers to recover the original information when source code is not available.
data InnerClassModifier = PublicInnerClassModifier -- ^ Marked or implicitly public in source.
  | PrivateInnerClassModifier -- ^ Marked private in source.
  | ProtectedInnerClassModifier -- ^ Marked protected in source.
  | StaticInnerClassModifier -- ^ Marked or implicitly static in source.
  | FinalInnerClassModifier -- ^ Marked final in source.
  | InterfaceInnerClassModifier -- ^ Was an interface in source.
  | AbstractInnerClassModifier -- ^ Marked or implicitly abstract in source.
  | SyntheticInnerClassModifier -- ^ Declared synthetic; not present in the source code.
  | AnnotationInnerClassModifier -- ^ Declared as an annotation type.
  | EnumInnerClassModifier -- ^ Declared as an enum type.
  deriving (Show, Eq, Ord)
