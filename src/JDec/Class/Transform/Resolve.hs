module JDec.Class.Transform.Resolve (
resolveClass,
resolveUTF8Constant,
resolveFieldNameAndType,
resolveMethodNameAndType,
resolveFieldRef,
resolveMethodRef,
resolveInterfaceMethodRef,
resolveStringConstant,
resolveIntegerConstant,
resolveLongConstant,
resolveFloatConstant,
resolveDoubleConstant,
resolveMethodHandle,
resolveMethodType,
resolveInvokeDynamic
) where

import JDec.Class.Raw.ConstantPoolIndex (ConstantPoolIndex, prettyPrint)
import JDec.Class.Raw.ConstantPoolEntry (ConstantPoolEntry(ClassConstantPoolEntry, UTF8ConstantPoolEntry, NameAndTypeConstantPoolEntry, FieldRefConstantPoolEntry, MethodRefConstantPoolEntry, InterfaceMethodRefConstantPoolEntry, StringConstantPoolEntry, IntegerConstantPoolEntry, LongConstantPoolEntry, FloatConstantPoolEntry, DoubleConstantPoolEntry, MethodHandleConstantPoolEntry, MethodTypeConstantPoolEntry, InvokeDynamicConstantPoolEntry))
import JDec.Util.ResultT (ResultT(Incorrect, Correct))
import JDec.Class.Typesafe.FieldNameAndType (FieldNameAndType(FieldNameAndType))
import JDec.Class.Typesafe.MethodNameAndType (MethodNameAndType(MethodNameAndType))
import JDec.Class.Typesafe.InterfaceMethodNameAndType (InterfaceMethodNameAndType(InterfaceMethodNameAndType))
import JDec.Class.Typesafe.FieldRef (FieldRef(FieldRef))
import JDec.Class.Typesafe.MethodRef (MethodRef(MethodRef))
import JDec.Class.Typesafe.InterfaceMethodRef (InterfaceMethodRef(InterfaceMethodRef))
import JDec.Class.Raw.MethodHandleKind (MethodHandleKind(GetFieldMethodHandleKind, GetStaticMethodHandleKind, PutFieldMethodHandleKind, PutStaticMethodHandleKind, InvokeVirtualMethodHandleKind, InvokeStaticMethodHandleKind, InvokeSpecialMethodHandleKind, NewInvokeSpecialMethodHandleKind, InvokeInterfaceMethodHandleKind))
import JDec.Class.Typesafe.MethodHandle (MethodHandle(GetFieldMethodHandle, GetStaticMethodHandle, PutFieldMethodHandle, PutStaticMethodHandle, InvokeVirtualMethodHandle, InvokeStaticMethodHandle, InvokeSpecialMethodHandle, NewInvokeSpecialMethodHandle, InvokeInterfaceMethodHandle))
import JDec.Class.Typesafe.InvokeDynamicInfo(InvokeDynamicInfo(InvokeDynamicInfo))
import JDec.Class.Typesafe.BootstrapMethodInfo(BootstrapMethodInfo)

import qualified Data.Map as M (Map, lookup)
import Data.Text(Text)
import Control.Applicative((<*>))
import Data.Functor((<$>))
import Data.List(genericLength, genericIndex)

-- | Get class name from constant pool by index
resolveClass :: M.Map ConstantPoolIndex ConstantPoolEntry -- ^ Constant pool
  -> ConstantPoolIndex -- ^ Index
  -> ResultT [String] Text -- ^ Class name or error list
resolveClass pool idx =
  case M.lookup idx pool of
    Just x ->
      case x of
        ClassConstantPoolEntry i -> resolveUTF8Constant pool i
        _ -> Incorrect [ "The pool entry at the index " ++ (prettyPrint idx) ++ " is not a class constant entry" ]
    Nothing -> Incorrect [ "The index " ++ (prettyPrint idx) ++ " is not in the constant pool" ]

-- | Get text from constant pool by index
resolveUTF8Constant :: M.Map ConstantPoolIndex ConstantPoolEntry -- ^ Constant pool
  -> ConstantPoolIndex -- ^ Index
  -> ResultT [String] Text -- ^ Text or error list
resolveUTF8Constant pool idx =
  case M.lookup idx pool of
    Just x ->
      case x of
        UTF8ConstantPoolEntry t -> Correct t
        _ -> Incorrect [ "The pool entry at the index " ++ (prettyPrint idx) ++ " is not an UTF-8 constant entry" ]
    Nothing -> Incorrect [ "The index " ++ (prettyPrint idx) ++ " is not in the constant pool" ]

-- | Get field name and type from constant pool by index
resolveFieldNameAndType :: M.Map ConstantPoolIndex ConstantPoolEntry -- ^ Constant pool
  -> ConstantPoolIndex -- ^ Index
  -> ResultT [String] FieldNameAndType -- ^ Field name and type or error list
resolveFieldNameAndType pool idx =
  case M.lookup idx pool of
    Just x ->
      case x of
        NameAndTypeConstantPoolEntry nameIdx descriptorIdx -> FieldNameAndType <$> resolveUTF8Constant pool nameIdx <*> resolveUTF8Constant pool descriptorIdx
        _ -> Incorrect [ "The pool entry at the index " ++ (prettyPrint idx) ++ " is not a name and type entry" ]
    Nothing -> Incorrect [ "The index " ++ (prettyPrint idx) ++ " is not in the constant pool" ]

-- | Get method name and type from constant pool by index
resolveMethodNameAndType :: M.Map ConstantPoolIndex ConstantPoolEntry -- ^ Constant pool
  -> ConstantPoolIndex -- ^ Index
  -> ResultT [String] MethodNameAndType -- ^ Method name and type or error list
resolveMethodNameAndType pool idx =
  case M.lookup idx pool of
    Just x ->
      case x of
        NameAndTypeConstantPoolEntry nameIdx descriptorIdx -> MethodNameAndType <$> resolveUTF8Constant pool nameIdx <*> resolveUTF8Constant pool descriptorIdx
        _ -> Incorrect [ "The pool entry at the index " ++ (prettyPrint idx) ++ " is not a name and type entry" ]
    Nothing -> Incorrect [ "The index " ++ (prettyPrint idx) ++ " is not in the constant pool" ]

-- | Get interface method name and type from constant pool by index
resolveInterfaceMethodNameAndType :: M.Map ConstantPoolIndex ConstantPoolEntry -- ^ Constant pool
  -> ConstantPoolIndex -- ^ Index
  -> ResultT [String] InterfaceMethodNameAndType -- ^ Interface method name and type or error list
resolveInterfaceMethodNameAndType pool idx =
  case M.lookup idx pool of
    Just x ->
      case x of
        NameAndTypeConstantPoolEntry nameIdx descriptorIdx -> InterfaceMethodNameAndType <$> resolveUTF8Constant pool nameIdx <*> resolveUTF8Constant pool descriptorIdx
        _ -> Incorrect [ "The pool entry at the index " ++ (prettyPrint idx) ++ " is not a name and type entry" ]
    Nothing -> Incorrect [ "The index " ++ (prettyPrint idx) ++ " is not in the constant pool" ]

-- | Get field ref from constant pool by index
resolveFieldRef :: M.Map ConstantPoolIndex ConstantPoolEntry -- ^ Constant pool
  -> ConstantPoolIndex -- ^ Index
  -> ResultT [String] FieldRef -- ^ Field ref or error list
resolveFieldRef pool idx =
  case M.lookup idx pool of
    Just x ->
      case x of
        FieldRefConstantPoolEntry classIdx nameAndTypeIdx -> FieldRef <$> resolveClass pool classIdx <*> resolveFieldNameAndType pool nameAndTypeIdx
        _ -> Incorrect [ "The pool entry at the index " ++ (prettyPrint idx) ++ " is not a field ref entry" ]
    Nothing -> Incorrect [ "The index " ++ (prettyPrint idx) ++ " is not in the constant pool" ]

-- | Get method ref from constant pool by index
resolveMethodRef :: M.Map ConstantPoolIndex ConstantPoolEntry -- ^ Constant pool
  -> ConstantPoolIndex -- ^ Index
  -> ResultT [String] MethodRef -- ^ Method ref or error list
resolveMethodRef pool idx =
  case M.lookup idx pool of
    Just x ->
      case x of
        MethodRefConstantPoolEntry classIdx nameAndTypeIdx -> MethodRef <$> resolveClass pool classIdx <*> resolveMethodNameAndType pool nameAndTypeIdx
        _ -> Incorrect [ "The pool entry at the index " ++ (prettyPrint idx) ++ " is not a method ref entry" ]
    Nothing -> Incorrect [ "The index " ++ (prettyPrint idx) ++ " is not in the constant pool" ]

-- | Get interface method ref from constant pool by index
resolveInterfaceMethodRef :: M.Map ConstantPoolIndex ConstantPoolEntry -- ^ Constant pool
  -> ConstantPoolIndex -- ^ Index
  -> ResultT [String] InterfaceMethodRef -- ^ Interface method ref or error list
resolveInterfaceMethodRef pool idx =
  case M.lookup idx pool of
    Just x ->
      case x of
        InterfaceMethodRefConstantPoolEntry classIdx nameAndTypeIdx -> InterfaceMethodRef <$> resolveClass pool classIdx <*> resolveInterfaceMethodNameAndType pool nameAndTypeIdx
        _ -> Incorrect [ "The pool entry at the index " ++ (prettyPrint idx) ++ " is not an interface method ref entry" ]
    Nothing -> Incorrect [ "The index " ++ (prettyPrint idx) ++ " is not in the constant pool" ]

-- | Get string constant from constant pool by index
resolveStringConstant :: M.Map ConstantPoolIndex ConstantPoolEntry -- ^ Constant pool
  -> ConstantPoolIndex -- ^ Index
  -> ResultT [String] Text -- ^ String constant or error list
resolveStringConstant pool idx =
  case M.lookup idx pool of
    Just x ->
      case x of
        StringConstantPoolEntry i -> resolveUTF8Constant pool i
        _ -> Incorrect [ "The pool entry at the index " ++ (prettyPrint idx) ++ " is not a string constant entry" ]
    Nothing -> Incorrect [ "The index " ++ (prettyPrint idx) ++ " is not in the constant pool" ]

-- | Get integer constant from constant pool by index
resolveIntegerConstant :: M.Map ConstantPoolIndex ConstantPoolEntry -- ^ Constant pool
  -> ConstantPoolIndex -- ^ Index
  -> ResultT [String] Integer -- ^ Integer constant or error list
resolveIntegerConstant pool idx =
  case M.lookup idx pool of
    Just x ->
      case x of
        IntegerConstantPoolEntry c -> Correct c
        _ -> Incorrect [ "The pool entry at the index " ++ (prettyPrint idx) ++ " is not an integer constant entry" ]
    Nothing -> Incorrect [ "The index " ++ (prettyPrint idx) ++ " is not in the constant pool" ]

-- | Get long constant from constant pool by index
resolveLongConstant :: M.Map ConstantPoolIndex ConstantPoolEntry -- ^ Constant pool
  -> ConstantPoolIndex -- ^ Index
  -> ResultT [String] Integer -- ^ Long constant or error list
resolveLongConstant pool idx =
  case M.lookup idx pool of
    Just x ->
      case x of
        LongConstantPoolEntry c -> Correct c
        _ -> Incorrect [ "The pool entry at the index " ++ (prettyPrint idx) ++ " is not a long constant entry" ]
    Nothing -> Incorrect [ "The index " ++ (prettyPrint idx) ++ " is not in the constant pool" ]

-- | Get float constant from constant pool by index
resolveFloatConstant :: M.Map ConstantPoolIndex ConstantPoolEntry -- ^ Constant pool
  -> ConstantPoolIndex -- ^ Index
  -> ResultT [String] Float -- ^ Float constant or error list
resolveFloatConstant pool idx =
  case M.lookup idx pool of
    Just x ->
      case x of
        FloatConstantPoolEntry c -> Correct c
        _ -> Incorrect [ "The pool entry at the index " ++ (prettyPrint idx) ++ " is not a float constant entry" ]
    Nothing -> Incorrect [ "The index " ++ (prettyPrint idx) ++ " is not in the constant pool" ]

-- | Get double constant from constant pool by index
resolveDoubleConstant :: M.Map ConstantPoolIndex ConstantPoolEntry -- ^ Constant pool
  -> ConstantPoolIndex -- ^ Index
  -> ResultT [String] Double -- ^ Double constant or error list
resolveDoubleConstant pool idx =
  case M.lookup idx pool of
    Just x ->
      case x of
        DoubleConstantPoolEntry c -> Correct c
        _ -> Incorrect [ "The pool entry at the index " ++ (prettyPrint idx) ++ " is not a double constant entry" ]
    Nothing -> Incorrect [ "The index " ++ (prettyPrint idx) ++ " is not in the constant pool" ]

-- | Get method handle from constant pool by index
resolveMethodHandle :: M.Map ConstantPoolIndex ConstantPoolEntry -- ^ Constant pool
  -> ConstantPoolIndex -- ^ Index
  -> ResultT [String] MethodHandle -- ^ Method handle or error list
resolveMethodHandle pool idx =
  case M.lookup idx pool of
    Just x ->
      case x of
        MethodHandleConstantPoolEntry handleKind refIdx ->
          case handleKind of
            GetFieldMethodHandleKind -> GetFieldMethodHandle <$> resolveFieldRef pool refIdx
            GetStaticMethodHandleKind -> GetStaticMethodHandle <$> resolveFieldRef pool refIdx
            PutFieldMethodHandleKind -> PutFieldMethodHandle <$> resolveFieldRef pool refIdx
            PutStaticMethodHandleKind -> PutStaticMethodHandle <$> resolveFieldRef pool refIdx
            InvokeVirtualMethodHandleKind -> InvokeVirtualMethodHandle <$> resolveMethodRef pool refIdx
            InvokeStaticMethodHandleKind -> InvokeStaticMethodHandle <$> resolveMethodRef pool refIdx
            InvokeSpecialMethodHandleKind -> InvokeSpecialMethodHandle <$> resolveMethodRef pool refIdx
            NewInvokeSpecialMethodHandleKind -> NewInvokeSpecialMethodHandle <$> resolveMethodRef pool refIdx
            InvokeInterfaceMethodHandleKind -> InvokeInterfaceMethodHandle <$> resolveInterfaceMethodRef pool refIdx
        _ -> Incorrect [ "The pool entry at the index " ++ (prettyPrint idx) ++ " is not a method handle entry" ]
    Nothing -> Incorrect [ "The index " ++ (prettyPrint idx) ++ " is not in the constant pool" ]

-- | Get method type from constant pool by index
resolveMethodType :: M.Map ConstantPoolIndex ConstantPoolEntry -- ^ Constant pool
  -> ConstantPoolIndex -- ^ Index
  -> ResultT [String] Text -- ^ Method type or error list
resolveMethodType pool idx =
  case M.lookup idx pool of
    Just x ->
      case x of
        MethodTypeConstantPoolEntry i -> resolveUTF8Constant pool i
        _ -> Incorrect [ "The pool entry at the index " ++ (prettyPrint idx) ++ " is not a method type entry" ]
    Nothing -> Incorrect [ "The index " ++ (prettyPrint idx) ++ " is not in the constant pool" ]

-- | Get invoke dynamic info from constant pool by index
resolveInvokeDynamic :: M.Map ConstantPoolIndex ConstantPoolEntry -- ^ Constant pool
  -> ConstantPoolIndex -- ^ Index
  -> [BootstrapMethodInfo] -- ^ Bootstrap methods list
  -> ResultT [String] InvokeDynamicInfo -- ^ Invoke dynamic info or error list
resolveInvokeDynamic pool idx bootstrapMethods =
  case M.lookup idx pool of
    Just x ->
      case x of
        InvokeDynamicConstantPoolEntry bootstrapIdx nameAndTypeIdx -> InvokeDynamicInfo <$> resolveBootstrapMethod bootstrapMethods bootstrapIdx <*> resolveMethodNameAndType pool nameAndTypeIdx
        _ -> Incorrect [ "The pool entry at the index " ++ (prettyPrint idx) ++ " is not an invoke dynamic entry" ]
    Nothing -> Incorrect [ "The index " ++ (prettyPrint idx) ++ " is not in the constant pool" ]

-- | Get bootstrap method from list by index
resolveBootstrapMethod :: [BootstrapMethodInfo] -- ^ List
  -> Integer -- ^ Index
  -> ResultT [String] BootstrapMethodInfo -- ^ Bootstrap method info or error list
resolveBootstrapMethod bootstrapMethods idx = if idx < genericLength bootstrapMethods then Correct (genericIndex bootstrapMethods idx) else Incorrect [ "The index " ++ (show idx) ++ " is not in the bootstrap methods array" ]
