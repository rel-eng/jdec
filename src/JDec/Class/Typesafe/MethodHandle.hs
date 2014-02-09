module JDec.Class.Typesafe.MethodHandle (
MethodHandle(GetFieldMethodHandle, GetStaticMethodHandle, PutFieldMethodHandle, PutStaticMethodHandle, InvokeVirtualMethodHandle, InvokeStaticMethodHandle, InvokeSpecialMethodHandle, NewInvokeSpecialMethodHandle, InvokeInterfaceMethodHandle),
getFieldRef,
getStaticFieldRef,
putFieldRef,
putStaticFieldRef,
invokeVirtualMethodRef,
invokeStaticMethodRef,
invokeSpecialMethodRef,
newInvokeSpecialMethodRef,
invokeInterfaceMethodRef
) where

import JDec.Class.Typesafe.FieldRef(FieldRef)
import JDec.Class.Typesafe.MethodRef(MethodRef)
import JDec.Class.Typesafe.InterfaceMethodRef(InterfaceMethodRef)

-- | A method handle. Method handle's kind indicates an equivalent instruction sequence of the method handle (it's bytecode behavior). Here C is the class or interface in which the field or method is to be found, f is the name of the field, m is the name of the method, T is the descriptor of the field or method, A* is the argument type sequence of the method
data MethodHandle = GetFieldMethodHandle { -- ^ getfield C.f:T
    getFieldRef :: FieldRef -- ^ A reference to a field.
  }
  | GetStaticMethodHandle { -- ^ getstatic C.f:T
    getStaticFieldRef :: FieldRef -- ^ A reference to a field.
  }
  | PutFieldMethodHandle { -- ^ putfield C.f:T
    putFieldRef :: FieldRef -- ^ A reference to a field.
  }
  | PutStaticMethodHandle { -- ^ putstatic C.f:T
    putStaticFieldRef :: FieldRef -- ^ A reference to a field.
  }
  | InvokeVirtualMethodHandle { -- ^ invokevirtual C.m:(A*)T
    invokeVirtualMethodRef :: MethodRef -- ^ A reference to a class method. The name of the method must not be <init> or <clinit>.
  }
  | InvokeStaticMethodHandle { -- ^ invokestatic C.m:(A*)T
    invokeStaticMethodRef :: MethodRef -- ^ A reference to a class method. The name of the method must not be <init> or <clinit>.
  }
  | InvokeSpecialMethodHandle { -- ^ invokespecial C.m:(A*)T
    invokeSpecialMethodRef :: MethodRef -- ^ A reference to a class method. The name of the method must not be <init> or <clinit>.
  }
  | NewInvokeSpecialMethodHandle { -- ^ new C; dup; invokespecial C.<init>:(A*)void
    newInvokeSpecialMethodRef :: MethodRef -- ^ A reference to a class method. The name of the method must be <init>.
  }
  | InvokeInterfaceMethodHandle { -- ^ invokeinterface C.m:(A*)T
    invokeInterfaceMethodRef :: InterfaceMethodRef -- ^ A reference to an interface method. The name of the method must not be <init> or <clinit>.
  } deriving Show
