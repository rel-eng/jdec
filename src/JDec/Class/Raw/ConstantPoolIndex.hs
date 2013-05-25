module JDec.Class.Raw.ConstantPoolIndex (
ConstantPoolIndex(ConstantPoolIndex),
unConstantPoolIndex,
isZeroConstantPoolIndex
) where

-- | Index into constant pool
newtype ConstantPoolIndex = ConstantPoolIndex Integer deriving (Eq, Ord, Show)

-- | Returns the integer value of a constant pool index
unConstantPoolIndex :: ConstantPoolIndex -- ^ Constant pool index
                       -> Integer -- ^ The integer value of a constant pool index
unConstantPoolIndex (ConstantPoolIndex i) = i

-- | Check if the integer value of a constant pool index is zero
isZeroConstantPoolIndex :: ConstantPoolIndex -- ^ Constant pool index
                           -> Bool -- ^ True if the integer value of a constant pool index is zero, false otherwise
isZeroConstantPoolIndex (ConstantPoolIndex i) = (i == 0)
