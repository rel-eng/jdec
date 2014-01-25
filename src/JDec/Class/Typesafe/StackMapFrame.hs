module JDec.Class.Typesafe.StackMapFrame (
StackMapFrame(
SameFrame,
SameLocalsOneStackItemFrame,
SameLocalsOneStackItemFrameExtended,
ChopFrame,
SameFrameExtended,
AppendFrame,
FullFrame),
sameFrameOffsetDelta,
sameLocalsOneStackItemFrameOffsetDelta,
sameLocalsOneStackItemFrameStack,
sameLocalsOneStackItemFrameExtendedOffsetDelta,
sameLocalsOneStackItemFrameExtendedStack,
chopCount,
chopFrameOffsetDelta,
sameFrameExtendedOffsetDelta,
appendFrameOffsetDelta,
appendFrameLocals,
fullFrameOffsetDelta,
fullFrameLocals,
fullFrameStack
) where

import JDec.Class.Typesafe.VerificationTypeInfo (VerificationTypeInfo)

-- | Stack map frame. Each frame type specifies (explicitly or implicitly) a value, offset delta, that is used to calculate the actual bytecode offset at which a frame applies. The bytecode offset at which a frame applies is calculated by adding offset delta + 1 to the bytecode offset of the previous frame, unless the previous frame is the initial frame of the method, in which case the bytecode offset is offset delta. All frame types, even FullFrame, rely on the previous frame for some of their semantics. The initial frame is implicit, and computed from the method descriptor.
data StackMapFrame = SameFrame { -- ^ The frame has exactly the same locals as the previous stack map frame and that the number of stack items is zero.
    sameFrameOffsetDelta :: Integer -- ^ The offset delta value for the frame is the value of the tag item, frame type (0 - 63).
  }
  | SameLocalsOneStackItemFrame { -- ^ The frame has exactly the same locals as the previous stack map frame and that the number of stack items is 1.
    sameLocalsOneStackItemFrameOffsetDelta :: Integer, -- ^ The offset delta value for the frame is the value (frame type - 64). Frame type is in the range [64, 127].
    sameLocalsOneStackItemFrameStack :: VerificationTypeInfo -- ^ Verification type info for the one stack item.
  }
  | SameLocalsOneStackItemFrameExtended { -- ^ The frame has exactly the same locals as the previous stack map frame and that the number of stack items is 1.
    sameLocalsOneStackItemFrameExtendedOffsetDelta :: Integer, -- ^ Offset delta.
    sameLocalsOneStackItemFrameExtendedStack :: VerificationTypeInfo -- ^ Verification type info for the one stack item.
  }
  | ChopFrame { -- ^ The operand stack is empty and the current locals are the same as the locals in the previous frame, except that the chopCount last locals are absent. The value of k is given by the formula 251 - frame_type.
    chopCount :: Integer, -- ^ The value is given by the formula 251 - frame type. Frame type is in the range [248-250].
    chopFrameOffsetDelta :: Integer -- Offset delta.
  }
  | SameFrameExtended { -- ^ The frame has exactly the same locals as the previous stack map frame and that the number of stack items is zero.
    sameFrameExtendedOffsetDelta :: Integer -- ^ Offset delta.
  }
  | AppendFrame { -- ^ The operand stack is empty and the current locals are the same as the locals in the previous frame, except that k additional locals are defined. The value of k is given by the formula frame type - 251. Frame type is in the range [252-254].
    appendFrameOffsetDelta :: Integer, -- ^ Offset delta
    appendFrameLocals :: [VerificationTypeInfo] -- ^ The 0th entry in locals represents the type of the first additional local variable. If Mth entry represents local variable N, then (M+1)th entry represents local variable N+1 if Mth entry is one of: TopVariableInfo, IntegerVariableInfo, FloatVariableInfo, NullVariableInfo, UninitializedThisVariableInfo, ObjectVariableInfo, UninitializedVariableInfo. Otherwise (M+1)th entry represents local variable N+2. It is an error if, for any index i, ith entry represents a local variable whose index is greater than the maximum number of local variables for the method.
  }
  | FullFrame { -- ^ Full frame.
    fullFrameOffsetDelta :: Integer, -- ^ Offset delta.
    fullFrameLocals :: [VerificationTypeInfo], -- ^ The 0th entry in locals represents the type of local variable 0. If Mth entry represents local variable N, then (M+1)th entry represents local variable N+1 if Mth entry is one of: TopVariableInfo, IntegerVariableInfo, FloatVariableInfo, NullVariableInfo, UninitializedThisVariableInfo, ObjectVariableInfo, UninitializedVariableInfo. Otherwise (M+1)th entry represents local variable N+2. It is an error if, for any index i, ith entry represents a local variable whose index is greater than the maximum number of local variables for the method.
    fullFrameStack :: [VerificationTypeInfo] -- ^ The 0th entry in stack represents the type of the bottom of the stack, and subsequent entries represent types of stack elements closer to the top of the operand stack. The bottom element of the stack will be referred to as stack element 0, and to subsequent elements as stack element 1, 2 etc. If Mth entry represents stack element N, then (M+1)th entry represents stack element N+1 if Mth entry is one of: TopVariableInfo, IntegerVariableInfo, FloatVariableInfo, NullVariableInfo, UninitializedThisVariableInfo, ObjectVariableInfo, UninitializedVariableInfo. Otherwise, (M+1th entry represents stack element N+2. It is an error if, for any index i, ith entry represents a stack entry whose index is greater than the maximum operand stack size for the method.
  } deriving Show 
