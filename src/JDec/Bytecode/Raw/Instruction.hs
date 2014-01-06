module JDec.Bytecode.Raw.Instruction where

import JDec.Class.Raw.ConstantPoolIndex (ConstantPoolIndex)
import JDec.Bytecode.Raw.ArrayType (ArrayType)

data Instruction = Aaload
  | Aastore
  | AconstNull
  | Aload {
    aloadIndex :: Integer
  }
  | AloadZero
  | AloadOne
  | AloadTwo
  | AloadThree
  | Anewarray {
    anewarrayIndex :: ConstantPoolIndex
  }
  | Areturn
  | Arraylength
  | Astore {
    astoreIndex :: Integer
  }
  | AstoreZero
  | AstoreOne
  | AstoreTwo
  | AstoreThree
  | Athrow
  | Baload
  | Bastore
  | Bipush {
    bipushImmediate :: Integer
  }
  | Caload
  | Castore
  | Checkcast {
    checkcastIndex :: ConstantPoolIndex
  }
  | DoubleToFloat
  | DoubleToInt
  | DoubleToLong
  | Dadd
  | Daload
  | Dastore
  | Dcmpg
  | Dcmpl
  | DconstZero
  | DconstOne
  | Ddiv
  | Dload {
    dloadIndex :: Integer
  }
  | DloadZero
  | DloadOne
  | DloadTwo
  | DloadThree
  | Dmul
  | Dneg
  | Drem
  | Dreturn
  | Dstore {
    dstoreIndex :: Integer
  }
  | DstoreZero
  | DstoreOne
  | DstoreTwo
  | DstoreThree
  | Dsub
  | Dup
  | DupXOne
  | DupXTwo
  | DupTwo
  | DupTwoXOne
  | DupTwoXTwo
  | FloatToDouble
  | FloatToInt
  | FloatToLong
  | Fadd
  | Faload
  | Fastore
  | Fcmpg
  | Fcmpl
  | FconstZero
  | FconstOne
  | FconstTwo
  | Fdiv
  | Fload {
    floadIndex :: Integer
  }
  | FloadZero
  | FloadOne
  | FloadTwo
  | FloadThree
  | Fmul
  | Fneg
  | Frem
  | Freturn
  | Fstore {
    fstoreIndex :: Integer
  }
  | FstoreZero
  | FstoreOne
  | FstoreTwo
  | FstoreThree
  | Fsub
  | Getfield {
    getfieldIndex :: ConstantPoolIndex
  }
  | Getstatic {
    getstaticIndex :: ConstantPoolIndex
  }
  | Goto {
    gotoBranchOffset :: Integer
  }
  | Gotow {
    gotowBranchOffset :: Integer
  }
  | IntToByte
  | IntToChar
  | IntToDouble
  | IntToFloat
  | IntToLong
  | IntToShort
  | Iadd
  | Iaload
  | Iand
  | Iastore
  | IconstMinusOne
  | IconstZero
  | IconstOne
  | IconstTwo
  | IconstThree
  | IconstFour
  | IconstFive
  | Idiv
  | IfAcmpeq {
    ifacmpeqBranchOffset :: Integer
  }
  | IfAcmpne {
    ifacmpneBranchOffset :: Integer
  }
  | IfIcmpeq {
    ificmpeqBranchOffset :: Integer
  }
  | IfIcmpne {
    ificmpneBranchOffset :: Integer
  }
  | IfIcmplt {
    ificmpltBranchOffset :: Integer
  }
  | IfIcmpge {
    ificmpgeBranchOffset :: Integer
  }
  | IfIcmpgt {
    ificmpgtBranchOffset :: Integer
  }
  | IfIcmple {
    ificmpleBranchOffset :: Integer
  }
  | Ifeq {
    ifeqBranchOffset :: Integer
  }
  | Ifne {
    ifneBranchOffset :: Integer
  }
  | Iflt {
    ifltBranchOffset :: Integer
  }
  | Ifge {
    ifgeBranchOffset :: Integer
  }
  | Ifgt {
    ifgtBranchOffset :: Integer
  }
  | Ifle {
    ifleBranchOffset :: Integer
  }
  | Ifnonnull {
    ifnonnullBranchOffset :: Integer
  }
  | Ifnull {
    ifnullBranchOffset :: Integer
  }
  | Iinc {
    iincIndex :: Integer,
    iincImmediate :: Integer
  }
  | Iload {
    iloadIndex :: Integer
  }
  | IloadZero
  | IloadOne
  | IloadTwo
  | IloadThree
  | Imul
  | Ineg
  | Instanceof {
    instanceofIndex :: ConstantPoolIndex
  }
  | Invokedynamic {
    invokedynamicIndex :: ConstantPoolIndex
  }
  | Invokeinterface {
    invokeinterfaceIndex :: ConstantPoolIndex,
    invokeinterfaceCount :: Integer
  }
  | Invokespecial {
    invokespecialIndex :: ConstantPoolIndex
  }
  | Invokestatic {
    invokestaticIndex :: ConstantPoolIndex
  }
  | Invokevirtual {
    invokevirtualIndex :: ConstantPoolIndex
  }
  | Ior
  | Irem
  | Ireturn
  | Ishl
  | Ishr
  | Istore {
    istoreIndex :: Integer
  }
  | IstoreZero
  | IstoreOne
  | IstoreTwo
  | IstoreThree
  | Isub
  | Iushr
  | Ixor
  | Jsr {
    jsrBranchOffset :: Integer
  }
  | Jsrw {
    jsrwBranchOffset :: Integer
  }
  | LongToDouble
  | LongToFloat
  | LongToInt
  | Ladd
  | Laload
  | Land
  | Lastore
  | Lcmp
  | LconstZero
  | LconstOne
  | Ldc {
    ldcIndex :: ConstantPoolIndex
  }
  | Ldcw {
    ldcwIndex :: ConstantPoolIndex
  }
  | LdcTwoW {
    ldcTwoWIndex :: ConstantPoolIndex
  }
  | Ldiv
  | Lload {
    lloadIndex :: Integer
  }
  | LloadZero
  | LloadOne
  | LloadTwo
  | LloadThree
  | Lmul
  | Lneg
  | Lookupswitch {
    lookupswitchDefault :: Integer,
    lookupswitchMatchOffsetPairs :: [(Integer, Integer)]
  }
  | Lor
  | Lrem
  | Lreturn
  | Lshl
  | Lshr
  | Lstore {
    lstoreIndex :: Integer
  }
  | LstoreZero
  | LstoreOne
  | LstoreTwo
  | LstoreThree
  | Lsub
  | Lushr
  | Lxor
  | Monitorenter
  | Monitorexit
  | Multianewarray {
    multianewarrayIndex :: ConstantPoolIndex,
    multianewarrayDimensions :: Integer
  }
  | New {
    newIndex :: ConstantPoolIndex
  }
  | NewArray {
    newArrayType :: ArrayType
  }
  | Nop
  | Pop
  | PopTwo
  | Putfield {
    putfieldIndex :: ConstantPoolIndex
  }
  | Putstatic {
    putstaticIndex :: ConstantPoolIndex
  }
  | Ret {
    retIndex :: Integer
  }
  | Return
  | Saload
  | Sastore
  | Sipush {
    sipushImmediate :: Integer
  }
  | Swap
  | Tableswitch {
    tableswitchDefault :: Integer,
    tableswitchLow :: Integer,
    tableswitchHigh :: Integer,
    tableswitchJumpOffsets :: [Integer]
  }
  | WideIload {
    wideIloadIndex :: Integer
  }
  | WideFload {
    wideFloadIndex :: Integer
  }
  | WideAload {
    wideIloadIndex :: Integer
  }
  | WideLload {
    wideIloadIndex :: Integer
  }
  | WideDload {
    wideIloadIndex :: Integer
  }
  | WideIstore {
    wideIstoreIndex :: Integer
  }
  | WideFstore {
    wideFstoreIndex :: Integer
  }
  | WideAstore {
    wideAstoreIndex :: Integer
  }
  | WideLstore {
    wideLstoreIndex :: Integer
  }
  | WideDstore {
    wideDstoreIndex :: Integer
  }
  | WideRet {
    wideRetIndex :: Integer
  }
  | WideIinc {
    wideIincIndex :: Integer,
    wideIincImmediate :: Integer
  }
  | Impdep1
  | Impdep2
  | Breakpoint
  deriving Show
