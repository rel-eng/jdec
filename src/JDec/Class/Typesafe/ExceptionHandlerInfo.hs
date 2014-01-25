module JDec.Class.Typesafe.ExceptionHandlerInfo (
ExceptionHandlerInfo(ExceptionHandlerInfo),
startPC,
endPC,
handlerPC,
catchType
) where

import Data.Text(Text)

-- | Describes one exception handler in the code array.
data ExceptionHandlerInfo = ExceptionHandlerInfo {
  startPC :: Integer, -- ^ The values of the two items startPC and endPC indicate the ranges in the code array at which the exception handler is active. The value of startPC must be a valid index into the code array of the opcode of an instruction. The value of endPC either must be a valid index into the code array of the opcode of an instruction or must be equal to code length, the length of the code array. The value of startPC must be less than the value of endPC. The startPC is inclusive and endPC is exclusive; that is, the exception handler must be active while the program counter is within the interval [startPC, endPC).
  endPC :: Integer, -- ^ End PC
  handlerPC :: Integer, -- ^ The value indicates the start of the exception handler. The value of the item must be a valid index into the code array and must be the index of the opcode of an instruction.
  catchType :: Maybe Text -- ^ A class of exceptions that this exception handler is designated to catch. The exception handler will be called only if the thrown exception is an instance of the given class or one of its subclasses. Otherwise this exception handler is called for all exceptions.
} deriving Show 
