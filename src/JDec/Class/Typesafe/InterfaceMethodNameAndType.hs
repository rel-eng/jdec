module JDec.Class.Typesafe.InterfaceMethodNameAndType (
InterfaceMethodNameAndType(InterfaceMethodNameAndType),
name,
descriptor
) where

import Data.Text(Text)

-- | Represents a method, without indicating which class or interface type it belongs to.
data InterfaceMethodNameAndType = InterfaceMethodNameAndType {
  name :: Text, -- ^ Either the special method name <init> or a valid unqualified name denoting a method.
  descriptor :: Text -- ^ A valid method descriptor.
} deriving Show
