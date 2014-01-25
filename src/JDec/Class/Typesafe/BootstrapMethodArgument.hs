module JDec.Class.Typesafe.BootstrapMethodArgument (
BootstrapMethodArgument(StringArgument, ClassArgument, IntegerArgument, LongArgument, FloatArgument, DoubleArgument, MethodHandleArgument, MethodTypeArgument),
stringValue,
classValue,
integerValue,
longValue,
floatValue,
doubleValue,
methodHandleValue,
methodTypeDescriptorValue
) where

import JDec.Class.Typesafe.MethodHandle(MethodHandle)

import Data.Text(Text)

-- | An argument for the bootstrap method.
data BootstrapMethodArgument = StringArgument { -- ^ String argument
    stringValue :: Text -- ^ Constant string value
  }
  | ClassArgument { -- ^ Class argument
    classValue :: Text -- ^ A valid binary class or interface name encoded in internal form
  }
  | IntegerArgument { -- ^ Integer argument
    integerValue :: Integer -- ^ Constant integer value
  }
  | LongArgument { -- ^ Long argument
    longValue :: Integer -- ^ Constant long value
  }
  | FloatArgument { -- ^ Float argument
    floatValue :: Float -- ^ Constant float value
  }
  | DoubleArgument { -- ^ Double argument
    doubleValue :: Double -- ^ Constant double value
  }
  | MethodHandleArgument { -- ^ Method handle argument
    methodHandleValue :: MethodHandle -- ^ Method handle
  }
  | MethodTypeArgument { -- ^ Method type argument
    methodTypeDescriptorValue :: Text -- ^ Method descriptor
  } deriving Show
