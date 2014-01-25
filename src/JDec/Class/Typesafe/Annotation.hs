module JDec.Class.Typesafe.Annotation (
Annotation(Annotation),
AnnotationElementValue(ConstByteValue, ConstCharValue, ConstDoubleValue, ConstFloatValue, ConstIntegerValue, ConstLongValue, ConstShortValue, ConstBooleanValue, ConstStringValue, EnumConstValue, ClassValue, AnnotationValue, ArrayValue),
annotationType,
annotationParameters,
constByteValue,
constCharValue,
constDoubleValue,
constFloatValue,
constIntegerValue,
constLongValue,
constShortValue,
constBooleanValue,
constStringValue,
enumTypeName,
enumConstName,
className,
annotationValue,
arrayValues
) where

import Data.Text(Text)

-- | An annotation on a program element
data Annotation = Annotation {
  annotationType :: Text, -- ^ A field descriptor representing the annotation type corresponding to the annotation represented by this annotation structure.
  annotationParameters :: [(Text, AnnotationElementValue)] -- ^ Each value represents a single element-value pair in the annotation.
} deriving Show

-- | Represents the value of the annotation element-value pair
data AnnotationElementValue = ConstByteValue { -- ^ Signed byte.
    constByteValue :: Integer -- ^ Value.
  }
  | ConstCharValue { -- ^  Unicode character code point in the Basic Multilingual Plane, encoded with UTF-16.
    constCharValue :: Text -- ^ Value.
  }
  | ConstDoubleValue { -- ^  Double-precision floating-point value.
    constDoubleValue :: Double -- ^ Value.
  }
  | ConstFloatValue { -- ^  Single-precision floating-point value.
    constFloatValue :: Float -- ^ Value.
  }
  | ConstIntegerValue { -- ^ Integer.
    constIntegerValue :: Integer -- ^ Value.
  }
  | ConstLongValue { -- ^ Long integer.
    constLongValue :: Integer -- ^ Value.
  }
  | ConstShortValue { -- ^ Signed short.
    constShortValue :: Integer -- ^ Value.
  }
  | ConstBooleanValue { -- ^ True or false.
    constBooleanValue :: Bool -- ^ Value.
  }
  | ConstStringValue { -- ^ String.
    constStringValue :: Text -- ^ Value.
  }
  | EnumConstValue { -- ^ Enum constant
    enumTypeName :: Text, -- ^ A valid field descriptor that denotes the internal form of the binary name of the type of the enum constant represented by this element value.
    enumConstName :: Text -- ^ The simple name of the enum constant represented by this element value.
  }
  | ClassValue { -- ^ Class
    className :: Text -- ^ The return descriptor of the type that is reified by the class represented by this element value.
  }
  | AnnotationValue { -- ^ Annotation type
    annotationValue :: Annotation -- ^ The element value represents a "nested" annotation.
  }
  | ArrayValue { -- ^ Array
    arrayValues :: [AnnotationElementValue] -- ^ Each value gives the value of an element of the array-typed value represented by this element value.
  } deriving Show
