module JDec.Util.ResultT (
ResultT(Incorrect, Correct)
) where

import Control.Applicative(Applicative, pure, (<*>))
import Data.Monoid(Monoid, mappend)

data ResultT a b = Incorrect a | Correct b deriving (Eq, Ord, Read, Show)

instance Functor (ResultT a) where
  fmap _ (Incorrect x) = Incorrect x
  fmap f (Correct y) = Correct (f y)

instance (Monoid a) => Applicative (ResultT a) where
  pure = Correct
  (<*>) (Incorrect e2) (Incorrect e1) = Incorrect (e1 `mappend` e2)
  (<*>) (Incorrect e) _ = Incorrect e
  (<*>) _ (Incorrect e) = Incorrect e
  (<*>) (Correct fun) (Correct arg) = Correct (fun arg)
