module JDec.Class.Typesafe.FieldRef (
FieldRef(FieldRef),
fieldClass,
fieldNameAndType
) where

import JDec.Class.Typesafe.FieldNameAndType(FieldNameAndType())

import Data.Text(Text)

-- | A reference to a class or interface field
data FieldRef = FieldRef {
  fieldClass :: Text, -- ^ A class or interface type that has the field as a member.
  fieldNameAndType :: FieldNameAndType -- ^ The name and descriptor of the field.
} deriving Show
