module JDec.Class.Typesafe.FieldNameAndType (
FieldNameAndType(FieldNameAndType),
name,
descriptor
) where

import Data.Text(Text)

-- | Represents a field, without indicating which class or interface type it belongs to.
data FieldNameAndType = FieldNameAndType {
  name :: Text, -- ^ Valid unqualified name denoting a field.
  descriptor :: Text -- ^ A valid field descriptor.
} deriving Show
