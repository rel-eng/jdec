module JDec.Class.Typesafe.EnclosingMethodInfo (
EnclosingMethodInfo(EnclosingMethodInfo),
className,
methodNameAndType
) where

import JDec.Class.Typesafe.MethodNameAndType(MethodNameAndType)

import Data.Text(Text)

-- | Enclosing method info for a local class or an anonymous class
data EnclosingMethodInfo = EnclosingMethodInfo {
  className :: Text, -- ^ The name of the innermost class that encloses the declaration of the current class.
  methodNameAndType :: Maybe MethodNameAndType -- ^ None if the current class is not immediately enclosed by a method or constructor. The name and type of a method in the class referenced by the enclosingMethodClassIndex otherwise.
} deriving Show
