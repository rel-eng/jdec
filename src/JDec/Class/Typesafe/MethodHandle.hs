module JDec.Class.Typesafe.MethodHandle (
MethodHandle(GetFieldMethodHandle, GetStaticMethodHandle, PutFieldMethodHandle, PutStaticMethodHandle, InvokeVirtualMethodHandle, InvokeStaticMethodHandle, InvokeSpecialMethodHandle, NewInvokeSpecialMethodHandle, InvokeInterfaceMethodHandle),
getFieldClass,
getFieldNameAndType,
getStaticFieldClass,
getStaticFieldNameAndType,
putFieldClass,
putFieldNameAndType,
putStaticFieldClass,
putStaticFieldNameAndType,
invokeVirtualMethodClass,
invokeVirtualMethodNameAndType,
invokeStaticMethodClass,
invokeStaticMethodNameAndType,
invokeSpecialMethodClass,
invokeSpecialMethodNameAndType,
newInvokeSpecialMethodClass,
newInvokeSpecialMethodNameAndType,
invokeInterfaceMethodClass,
invokeInterfaceMethodNameAndType
) where

import JDec.Class.Typesafe.MethodNameAndType(MethodNameAndType)
import JDec.Class.Typesafe.FieldNameAndType(FieldNameAndType)

import Data.Text(Text)

-- | A method handle. Method handle's kind indicates an equivalent instruction sequence of the method handle (it's bytecode behavior). Here C is the class or interface in which the field or method is to be found, f is the name of the field, m is the name of the method, T is the descriptor of the field or method, A* is the argument type sequence of the method
data MethodHandle = GetFieldMethodHandle { -- ^ getfield C.f:T
    getFieldClass :: Text, -- ^ A class or interface type that has the field as a member.
    getFieldNameAndType :: FieldNameAndType -- ^ The name and descriptor of the field.
  }
  | GetStaticMethodHandle { -- ^ getstatic C.f:T
    getStaticFieldClass :: Text, -- ^ A class or interface type that has the field as a member.
    getStaticFieldNameAndType :: FieldNameAndType -- ^ The name and descriptor of the field.
  }
  | PutFieldMethodHandle { -- ^ putfield C.f:T
    putFieldClass :: Text, -- ^ A class or interface type that has the field as a member.
    putFieldNameAndType :: FieldNameAndType -- ^ The name and descriptor of the field.
  }
  | PutStaticMethodHandle { -- ^ putstatic C.f:T
    putStaticFieldClass :: Text, -- ^ A class or interface type that has the field as a member.
    putStaticFieldNameAndType :: FieldNameAndType -- ^ The name and descriptor of the field.
  }
  | InvokeVirtualMethodHandle { -- ^ invokevirtual C.m:(A*)T
    invokeVirtualMethodClass :: Text, -- ^ A class type that has the method as a member.
    invokeVirtualMethodNameAndType :: MethodNameAndType -- ^ The name and descriptor of the method. The name of the method must not be <init> or <clinit>.
  }
  | InvokeStaticMethodHandle { -- ^ invokestatic C.m:(A*)T
    invokeStaticMethodClass :: Text, -- ^ A class type that has the method as a member.
    invokeStaticMethodNameAndType :: MethodNameAndType -- ^ The name and descriptor of the method. The name of the method must not be <init> or <clinit>.
  }
  | InvokeSpecialMethodHandle { -- ^ invokespecial C.m:(A*)T
    invokeSpecialMethodClass :: Text, -- ^ A class type that has the method as a member.
    invokeSpecialMethodNameAndType :: MethodNameAndType -- ^ The name and descriptor of the method. The name of the method must not be <init> or <clinit>.
  }
  | NewInvokeSpecialMethodHandle { -- ^ new C; dup; invokespecial C.<init>:(A*)void
    newInvokeSpecialMethodClass :: Text, -- ^ A class type that has the method as a member.
    newInvokeSpecialMethodNameAndType :: MethodNameAndType -- ^ The name and descriptor of the method. The name of the method must be <init>.
  }
  | InvokeInterfaceMethodHandle { -- ^ invokeinterface C.m:(A*)T
    invokeInterfaceMethodClass :: Text, -- ^ An interface type that has the method as a member.
    invokeInterfaceMethodNameAndType :: MethodNameAndType -- ^ The name and descriptor of the method. The name of the method must not be <init> or <clinit>.
  } deriving Show
