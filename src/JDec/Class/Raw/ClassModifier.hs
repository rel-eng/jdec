module JDec.Class.Raw.ClassModifier (
ClassModifier(
  PublicClassModifier,
  FinalClassModifier,
  SuperClassModifier,
  InterfaceClassModifier,
  AbstractClassModifier,
  SyntheticClassModifier,
  AnnotationClassModifier,
  EnumClassModifier)
) where

-- | These flags are used to denote access permissions to and properties of the class or interface.
data ClassModifier = PublicClassModifier -- ^ Declared public; may be accessed from outside its package.
  | FinalClassModifier -- ^ Declared final; no subclasses allowed.
  | SuperClassModifier -- ^ Treat superclass methods specially when invoked by the invokespecial instruction.
  | InterfaceClassModifier -- ^ Is an interface, not a class. If the InterfaceClassModifier flag of the class file is set, its AbstractClassModifier flag must also be set. Such a class file must not have its FinalClassModifier, SuperClassModifier or EnumClassModifier flags set.
  | AbstractClassModifier -- ^ Declared abstract; must not be instantiated.
  | SyntheticClassModifier -- ^ Declared synthetic; not present in the source code.
  | AnnotationClassModifier -- ^ Declared as an annotation type. If the AnnotationClassModifier flag is set, the InterfaceClassModifier flag must be set as well. If the InterfaceClassModifier flag of the class file is not set, it may have any of the other flags set, except the AnnotationClassModifier flag. However, such a class file cannot have both its FinalClassModifier and AbstractClassModifier flags set.
  | EnumClassModifier -- ^ Declared as an enum type.
  deriving (Show, Eq, Ord)
