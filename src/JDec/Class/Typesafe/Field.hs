module JDec.Class.Typesafe.Field (
Field(Field),
name,
descriptor,
accessFlags,
constantValue,
synthetic,
signature,
deprecated,
runtimeVisibleAnnotations,
runtimeInvisibleAnnotations
) where

import JDec.Class.Raw.FieldModifier(FieldModifier)
import JDec.Class.Typesafe.ConstantFieldValue(ConstantFieldValue)
import JDec.Class.Typesafe.Annotation(Annotation)

import Data.Set(Set)
import Data.Text(Text)

-- | Field decription. No two fields in one class file may have the same name and descriptor.
data Field = Field {
  name :: Text, -- ^ A valid unqualified name denoting a field.
  descriptor :: Text, -- ^ A valid field descriptor.
  accessFlags :: Set FieldModifier, -- ^ The value is a set of flags used to denote access permission to and properties of the field.
  constantValue :: Maybe ConstantFieldValue, -- ^ Optional constant value
  synthetic :: Bool, -- ^ Is this field synthetic?
  signature :: Maybe Text, -- ^ A generic signature information if a generic signature of the field includes references to type variables or parameterized types
  deprecated :: Bool, -- ^ Is this field deprecated?
  runtimeVisibleAnnotations :: [Annotation], -- ^ Runtime visible annotations
  runtimeInvisibleAnnotations :: [Annotation] -- ^ Runtime invisible annotations
} deriving Show
