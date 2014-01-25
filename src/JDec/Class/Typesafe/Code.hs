module JDec.Class.Typesafe.Code (
Code(Code),
maxStack,
maxLocals,
exceptionHandlers,
lineNumbers,
localVariables,
localVariablesTypes,
stackMapFrames,
instructions,
) where

import JDec.Class.Typesafe.ExceptionHandlerInfo(ExceptionHandlerInfo)
import JDec.Class.Raw.LineNumberInfo(LineNumberInfo)
import JDec.Class.Typesafe.LocalVariableInfo(LocalVariableInfo)
import JDec.Class.Typesafe.LocalVariableTypeInfo(LocalVariableTypeInfo)
import JDec.Class.Typesafe.StackMapFrame(StackMapFrame)
import JDec.Bytecode.Raw.Instruction(Instruction)

import Data.Map(Map)

-- | The virtual machine instructions and auxiliary information for a single method, instance initialization method, or class or interface initialization method.
data Code = Code {
  maxStack :: Integer, -- ^ The value gives the maximum depth of the operand stack of this method at any point during execution of the method.
  maxLocals :: Integer, -- ^ The value gives the number of local variables in the local variable array allocated upon invocation of this method, including the local variables used to pass parameters to the method on its invocation. The greatest local variable index for a value of type long or double is maxLocals - 2. The greatest local variable index for a value of any other type is maxLocals - 1.
  exceptionHandlers :: [ExceptionHandlerInfo], -- ^ Each entry describes one exception handler in the code array. The order of the handlers is significant.
  lineNumbers :: [LineNumberInfo], -- ^ Line number info
  localVariables :: [LocalVariableInfo], -- ^ Local variables info
  localVariablesTypes :: [LocalVariableTypeInfo], -- ^ Local variables type info
  stackMapFrames :: [StackMapFrame], -- ^ Stack map frames
  instructions :: Map Integer Instruction -- ^ Instructions
} deriving Show
