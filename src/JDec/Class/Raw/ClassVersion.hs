module JDec.Class.Raw.ClassVersion (
ClassVersion(ClassVersion), 
minorClassVersion,
majorClassVersion
) where

-- | The values of the minorVersion and majorVersion items are the minor and major version numbers of the class file. Together, a major and a minor version number determine the version of the class file format. If a class file has major version number M and minor version number m, we denote the version of its class file format as M.m.
data ClassVersion = ClassVersion {
  minorClassVersion :: Integer, -- ^ Minor version number of the class file
  majorClassVersion :: Integer -- ^ Major version number of the class file
} deriving Show
