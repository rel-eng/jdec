module JDec.Class.Typesafe.BootstrapMethodInfo (
BootstrapMethodInfo(BootstrapMethodInfo),
bootstrapMethodRef,
bootstrapArguments
) where

import JDec.Class.Typesafe.MethodHandle(MethodHandle)
import JDec.Class.Typesafe.BootstrapMethodArgument(BootstrapMethodArgument)

-- | Specifies a bootstrap method, and a sequence (perhaps empty) of static arguments for the bootstrap method.
data BootstrapMethodInfo = BootstrapMethodInfo {
  bootstrapMethodRef :: MethodHandle, -- ^ Method handle.
  bootstrapArguments :: [BootstrapMethodArgument] -- ^ Method arguments.
} deriving Show
