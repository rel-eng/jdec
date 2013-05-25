module JDec.Class.Raw.LineNumberInfo (
  LineNumberInfo(LineNumberInfo),
  lineNumberStartPC,
  lineNumber
) where

-- | It indicates that the line number in the original source file changes at a given point in the code array.
data LineNumberInfo = LineNumberInfo {
  lineNumberStartPC :: Integer, -- ^ The value must indicate the index into the code array at which the code for a new line in the original source file begins.
  lineNumber :: Integer -- ^ The value must give the corresponding line number in the original source file.
} deriving Show
