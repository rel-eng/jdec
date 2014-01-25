module JDec.Class.Typesafe.Method (
Method(Method),
name,
descriptor,
accessFlags,
exceptions,
synthetic,
signature,
deprecated,
runtimeVisibleAnnotations,
runtimeInvisibleAnnotations,
runtimeVisibleParameterAnnotations,
runtimeInvisibleParameterAnnotations,
annotationDefault,
code
) where

import JDec.Class.Raw.MethodModifier(MethodModifier)
import JDec.Class.Typesafe.Annotation(Annotation, AnnotationElementValue)
import JDec.Class.Typesafe.Code(Code)

import Data.Set(Set)
import Data.Text(Text)

-- | Each method, including each instance initialization method and the class or interface initialization method have such a description. No two methods in one class file may have the same name and descriptor.
data Method = Method {
  name :: Text, -- ^ Either one of the special method names <init> or <clinit>, or a valid unqualified name denoting a method.
  descriptor :: Text, -- ^ A valid method descriptor.
  accessFlags :: Set MethodModifier, -- ^ The value is a set of flags used to denote access permission to and properties of the method.
  exceptions :: [Text], -- Class types that this method is declared to throw
  synthetic :: Bool, -- ^ Is this method synthetic?
  signature :: Maybe Text, -- ^ A generic signature information if a generic signature of the method includes references to type variables or parameterized types
  deprecated :: Bool, -- ^ Is this method deprecated?
  runtimeVisibleAnnotations :: [Annotation], -- ^ Runtime visible annotations
  runtimeInvisibleAnnotations :: [Annotation], -- ^ Runtime invisible annotations
  runtimeVisibleParameterAnnotations :: [[Annotation]], -- ^ Runtime visible parameter annotations
  runtimeInvisibleParameterAnnotations :: [[Annotation]], -- ^ Runtime invisible parameter annotations
  annotationDefault :: Maybe AnnotationElementValue, -- ^ Optional default value for the elementof an annotation type represented by the method
  code :: Maybe Code -- ^ Code
} deriving Show
