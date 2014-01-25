module JDec.Class.Typesafe.VerificationTypeInfo (
VerificationTypeInfo(
TopVariableInfo,
IntegerVariableInfo,
FloatVariableInfo,
LongVariableInfo,
DoubleVariableInfo,
NullVariableInfo,
UninitializedThisVariableInfo,
ObjectVariableInfo,
UninitializedVariableInfo),
objectVariable,
uninitializedVariableOffset
) where

import Data.Text(Text)

-- | Specifies the verification type of one or two locations.
data VerificationTypeInfo = TopVariableInfo -- ^ Indicates that the local variable has the verification type top (⊤).
  | IntegerVariableInfo -- ^ Indicates that the location contains the verification type int.
  | FloatVariableInfo -- ^ Indicates that the location contains the verification type float.
  | LongVariableInfo -- ^ Indicates that the location contains the verification type long. If the location is a local variable, then it must not be the local variable with the highest index and the next higher numbered local variable contains the verification type ⊤. If the location is an operand stack entry, then the current location must not be the topmost location of the operand stack and the next location closer to the top of the operand stack contains the verification type ⊤. It gives the contents of two locations in the operand stack or in the local variables.
  | DoubleVariableInfo -- ^ Indicates that the location contains the verification type double. If the location is a local variable, then it must not be the local variable with the highest index and the next higher numbered local variable contains the verification type ⊤. If the location is an operand stack entry, then the current location must not be the topmost location of the operand stack and the next location closer to the top of the operand stack contains the verification type ⊤. It gives the contents of two locations in in the operand stack or in the local variables.
  | NullVariableInfo -- ^ Indicates that location contains the verification type null.
  | UninitializedThisVariableInfo -- ^ Indicates that the location contains the verification type uninitializedThis.
  | ObjectVariableInfo { -- ^ Indicates that the location contains an instance of the class.
    objectVariable :: Text -- ^ A valid binary class or interface name encoded in internal form
  }
  | UninitializedVariableInfo { -- ^ Indicates that the location contains the verification type uninitialized(offset).
    uninitializedVariableOffset :: Integer -- ^ Indicates the offset, in the code array of the Code attribute that contains this StackMapTable attribute, of the new instruction that created the object being stored in the location.
  } deriving Show 
