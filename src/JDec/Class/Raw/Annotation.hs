module JDec.Class.Raw.Annotation (
Annotation(Annotation),
AnnotationElementValue(
  AnnotationElementConstByteValue,
  AnnotationElementConstCharValue,
  AnnotationElementConstDoubleValue,
  AnnotationElementConstFloatValue,
  AnnotationElementConstIntegerValue,
  AnnotationElementConstLongValue,
  AnnotationElementConstShortValue,
  AnnotationElementConstBooleanValue,
  AnnotationElementConstStringValue,
  AnnotationElementEnumConstValue,
  AnnotationElementClassValue,
  AnnotationElementAnnotationValue,
  AnnotationElementArrayValue),
annotationTypeIndex,
annotationElementNameValuePairs,
annotationElementConstByteValueIndex,
annotationElementConstCharValueIndex,
annotationElementConstDoubleValueIndex,
annotationElementConstFloatValueIndex,
annotationElementConstIntegerValueIndex,
annotationElementConstLongValueIndex,
annotationElementConstShortValueIndex,
annotationElementConstBooleanValueIndex,
annotationElementConstStringValueIndex,
annotationElementTypeNameIndex,
annotationElementConstNameIndex,
annotationElementClassInfoIndex,
annotationElementAnnotationValue,
annotationElementArrayValues
) where

import JDec.Class.Raw.ConstantPoolIndex (ConstantPoolIndex)

-- | An annotation on a program element
data Annotation = Annotation {
  annotationTypeIndex :: ConstantPoolIndex, -- ^ The value must be a valid index into the constant pool table. The constant pool entry at that index must be a UTF8ConstantPoolEntry representing a field descriptor representing the annotation type corresponding to the annotation represented by this annotation structure.
  annotationElementNameValuePairs :: [(ConstantPoolIndex, AnnotationElementValue)] -- ^ Each value represents a single element-value pair in the annotation. The value of the pair index must be a valid index into the constant pool table. The constant pool entry at that index must be a UTF8ConstantPoolEntry representing a valid field descriptor that denotes the name of the annotation type element represented by this pair.
} deriving Show

-- | Represents the value of the annotation element-value pair
data AnnotationElementValue = AnnotationElementConstByteValue { -- ^ Signed byte
    annotationElementConstByteValueIndex :: ConstantPoolIndex -- ^ The value must be a valid index into the constant pool table. The constant pool entry at that index must be of the correct entry type for the field type.
  }
  | AnnotationElementConstCharValue { -- ^  Unicode character code point in the Basic Multilingual Plane, encoded with UTF-16
    annotationElementConstCharValueIndex :: ConstantPoolIndex -- ^ The value must be a valid index into the constant pool table. The constant pool entry at that index must be of the correct entry type for the field type.
  }
  | AnnotationElementConstDoubleValue { -- ^  Double-precision floating-point value
    annotationElementConstDoubleValueIndex :: ConstantPoolIndex -- ^ The value must be a valid index into the constant pool table. The constant pool entry at that index must be of the correct entry type for the field type.
  }
  | AnnotationElementConstFloatValue { -- ^  Single-precision floating-point value
    annotationElementConstFloatValueIndex :: ConstantPoolIndex -- ^ The value must be a valid index into the constant pool table. The constant pool entry at that index must be of the correct entry type for the field type.
  }
  | AnnotationElementConstIntegerValue { -- ^ Integer
    annotationElementConstIntegerValueIndex :: ConstantPoolIndex -- ^ The value must be a valid index into the constant pool table. The constant pool entry at that index must be of the correct entry type for the field type.
  }
  | AnnotationElementConstLongValue { -- ^ Long integer
    annotationElementConstLongValueIndex :: ConstantPoolIndex -- ^ The value must be a valid index into the constant pool table. The constant pool entry at that index must be of the correct entry type for the field type.
  }
  | AnnotationElementConstShortValue { -- ^ Signed short
    annotationElementConstShortValueIndex :: ConstantPoolIndex -- ^ The value must be a valid index into the constant pool table. The constant pool entry at that index must be of the correct entry type for the field type.
  }
  | AnnotationElementConstBooleanValue { -- ^ True or false
    annotationElementConstBooleanValueIndex :: ConstantPoolIndex -- ^ The value must be a valid index into the constant pool table. The constant pool entry at that index must be of the correct entry type for the field type.
  }
  | AnnotationElementConstStringValue { -- ^ String
    annotationElementConstStringValueIndex :: ConstantPoolIndex -- ^ The value must be a valid index into the constant pool table. The constant pool entry at that index must be of the correct entry type for the field type.
  }
  | AnnotationElementEnumConstValue { -- ^ Enum constant
    annotationElementTypeNameIndex :: ConstantPoolIndex, -- ^ The value must be a valid index into the constant pool table. The constant pool entry at that index must be a UTF8ConstantPoolEntry representing a valid field descriptor that denotes the internal form of the binary name of the type of the enum constant represented by this element value.
    annotationElementConstNameIndex :: ConstantPoolIndex -- ^ The value must be a valid index into the constant pool table. The constant pool entry at that index must be a UTF8ConstantPoolEntry representing the simple name of the enum constant represented by this element value.
  }
  | AnnotationElementClassValue { -- ^ Class
    annotationElementClassInfoIndex :: ConstantPoolIndex -- ^ The value must be a valid index into the constant pool table. The constant pool entry at that index must be a UTF8ConstantPoolEntry representing the return descriptor of the type that is reified by the class represented by this element value.
  }
  | AnnotationElementAnnotationValue { -- ^ Annotation type
    annotationElementAnnotationValue :: Annotation -- ^ The element value represents a "nested" annotation.
  }
  | AnnotationElementArrayValue { -- ^ Array
    annotationElementArrayValues :: [AnnotationElementValue] -- ^ Each value gives the value of an element of the array-typed value represented by this element value.
  } deriving Show