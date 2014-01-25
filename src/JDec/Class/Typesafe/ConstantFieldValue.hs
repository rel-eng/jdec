module JDec.Class.Typesafe.ConstantFieldValue (
ConstantFieldValue(IntegerValue, LongValue, FloatValue, DoubleValue, StringValue),
integerValue,
longValue,
floatValue,
doubleValue,
stringValue
) where

import Data.Text(Text)

-- | Constant value of the field
data ConstantFieldValue = IntegerValue { -- ^ Integer value
    integerValue :: Integer -- ^ Integer value
  }
  | LongValue { -- ^ Long value
    longValue :: Integer -- ^ Long value
  }
  | FloatValue { -- ^ Float value
    floatValue :: Float -- ^ Float value
  }
  | DoubleValue { -- ^ Double value
    doubleValue :: Double -- ^ Double value
  }
  | StringValue { -- ^ String value
    stringValue :: Text -- ^ String value
  } deriving Show
