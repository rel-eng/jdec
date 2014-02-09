module JDec.Class.Typesafe.InvokeDynamicInfo (
InvokeDynamicInfo(InvokeDynamicInfo),
bootstrapMethodInfo,
methodNameAndType
) where

import JDec.Class.Typesafe.BootstrapMethodInfo(BootstrapMethodInfo)
import JDec.Class.Typesafe.MethodNameAndType(MethodNameAndType)

-- | A reference to a class method
data InvokeDynamicInfo = InvokeDynamicInfo {
  bootstrapMethodInfo :: BootstrapMethodInfo, -- ^ Bootstrap method info.
  methodNameAndType :: MethodNameAndType -- ^ A method name and method descriptor.
} deriving Show 
