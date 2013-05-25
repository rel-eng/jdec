module JDec.Class.Raw.FieldModifier (
FieldModifier(
  PublicFieldModifier,
  PrivateFieldModifier,
  ProtectedFieldModifier,
  StaticFieldModifier,
  FinalFieldModifier,
  VolatileFieldModifier,
  TransientFieldModifier,
  SyntheticFieldModifier,
  EnumFieldModifier)
) where

-- | These flags are used to denote access permission to and properties of the field. Fields of classes may set any of the flags. However, a specific field of a class may have at most one of its PrivateFieldModifier, ProtectedFieldModifier, and PublicFieldModifier flags set and must not have both its FinalFieldModifier and VolatileFieldModifier flags set. All fields of interfaces must have their PublicFieldModifier, StaticFieldModifier, and FinalFieldModifier flags set; they may have their SyntheticFieldModifier flag set and must not have any of the other flags set.
data FieldModifier = PublicFieldModifier -- ^ Declared public; may be accessed from outside its package.
  | PrivateFieldModifier -- ^ Declared private; usable only within the defining class.
  | ProtectedFieldModifier -- ^ Declared protected; may be accessed within subclasses.
  | StaticFieldModifier -- ^ Declared static.
  | FinalFieldModifier -- ^ Declared final; never directly assigned to after object construction.
  | VolatileFieldModifier -- ^ Declared volatile; cannot be cached.
  | TransientFieldModifier -- ^ Declared transient; not written or read by a persistent object manager.
  | SyntheticFieldModifier -- ^ Declared synthetic; not present in the source code.
  | EnumFieldModifier -- ^ Declared as an element of an enum.
  deriving Show
