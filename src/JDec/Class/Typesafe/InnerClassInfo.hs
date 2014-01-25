module JDec.Class.Typesafe.InnerClassInfo (
InnerClassInfo(InnerClassInfo),
innerName,
outerName,
simpleInnerName,
accessFlags
) where

import JDec.Class.Raw.InnerClassModifier (InnerClassModifier)

import Data.Set(Set)
import Data.Text(Text)

-- | Inner class info which represents a class or interface C that is not a package member.
data InnerClassInfo = InnerClassInfo {
  innerName :: Text, -- ^ Name of the class or interface C.
  outerName :: Maybe Text, -- ^ None if C is not a member of a class or an interface (that is, if C is a top-level class or interface or a local class or an anonymous class). Name the class or interface of which C is a member otherwise.
  simpleInnerName :: Maybe Text, -- ^ None if C is anonymous. The original simple name of C, as given in the source code from which this class file was compiled otherwise.
  accessFlags :: Set InnerClassModifier -- ^ The value is a set of flags used to denote access permissions to and properties of class or interface C as declared in the source code from which this class file was compiled.
} deriving Show
