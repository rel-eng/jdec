module JDec.Class.Typesafe.InterfaceMethodRef (
InterfaceMethodRef(InterfaceMethodRef),
interfaceMethodClass,
interfaceMethodNameAndType
) where

import JDec.Class.Typesafe.InterfaceMethodNameAndType(InterfaceMethodNameAndType)

import Data.Text(Text)

-- | A reference to an interface method
data InterfaceMethodRef = InterfaceMethodRef {
  interfaceMethodClass :: Text, -- ^ An interface type that has the method as a member.
  interfaceMethodNameAndType :: InterfaceMethodNameAndType -- ^ The name and descriptor of the method.
} deriving Show
