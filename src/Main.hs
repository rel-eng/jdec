module Main where

import JDec.Class.Raw.Class
import JDec.Class.Parse.ParseClass
import JDec.Class.Raw.ConstantPoolEntry

import Data.ByteString.Lazy as BL (readFile)
import Data.Binary.Get (runGet)
import Control.Monad(forM_)
import Data.Map
import qualified Data.Text.IO as T

main :: IO ()
main = do
  return ()
