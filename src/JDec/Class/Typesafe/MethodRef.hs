module JDec.Class.Typesafe.MethodRef (
MethodRef(MethodRef),
methodClass,
methodNameAndType
) where

import JDec.Class.Typesafe.MethodNameAndType(MethodNameAndType)

import Data.Text(Text)

-- | A reference to a class method
data MethodRef = MethodRef {
  methodClass :: Text, -- ^ A class type that has the method as a member.
  methodNameAndType :: MethodNameAndType -- ^ The name and descriptor of the method.
} deriving Show
