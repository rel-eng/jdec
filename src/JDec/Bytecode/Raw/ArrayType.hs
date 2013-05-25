module JDec.Bytecode.Raw.ArrayType (
ArrayType(
ArrayTypeBoolean,
ArrayTypeChar,
ArrayTypeFloat,
ArrayTypeDouble,
ArrayTypeByte,
ArrayTypeShort,
ArrayTypeInt,
ArrayTypeLong)
) where

-- | Array type
data ArrayType = ArrayTypeBoolean -- ^ Array of boolean
  | ArrayTypeChar -- ^ Array of char
  | ArrayTypeFloat -- ^ Array of float
  | ArrayTypeDouble -- ^ Array of double
  | ArrayTypeByte -- ^ Array of byte
  | ArrayTypeShort -- ^ Array of short
  | ArrayTypeInt -- ^ Array of int
  | ArrayTypeLong -- ^ Array of long
  deriving Show
