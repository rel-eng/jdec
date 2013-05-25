module JDec.Class.Raw.MethodHandleKind (
MethodHandleKind(
  GetFieldMethodHandleKind,
  GetStaticMethodHandleKind,
  PutFieldMethodHandleKind,
  PutStaticMethodHandleKind,
  InvokeVirtualMethodHandleKind,
  InvokeStaticMethodHandleKind,
  InvokeSpecialMethodHandleKind,
  NewInvokeSpecialMethodHandleKind,
  InvokeInterfaceMethodHandleKind)
) where

-- | Method handle's kind indicates an equivalent instruction sequence of the method handle (it's bytecode behavior). Here C is the class or interface in which the field or method is to be found, f is the name of the field, m is the name of the method, T is the descriptor of the field or method, A* is the argument type sequence of the method
data MethodHandleKind = GetFieldMethodHandleKind -- ^ getfield C.f:T
  | GetStaticMethodHandleKind -- ^ getstatic C.f:T
  | PutFieldMethodHandleKind -- ^ putfield C.f:T
  | PutStaticMethodHandleKind -- ^ putstatic C.f:T
  | InvokeVirtualMethodHandleKind -- ^ invokevirtual C.m:(A*)T
  | InvokeStaticMethodHandleKind -- ^ invokestatic C.m:(A*)T
  | InvokeSpecialMethodHandleKind -- ^ invokespecial C.m:(A*)T
  | NewInvokeSpecialMethodHandleKind -- ^ new C; dup; invokespecial C.<init>:(A*)void
  | InvokeInterfaceMethodHandleKind -- ^ invokeinterface C.m:(A*)T
  deriving Show
