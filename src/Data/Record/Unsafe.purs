module Data.Record.Unsafe
  ( unsafeGetFn
  , unsafeSetFn
  , unsafeDeleteFn
  , unsafeHasFn
  , unsafeGet
  , unsafeSet
  , unsafeDelete
  , unsafeHas
  ) where

import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)

foreign import unsafeGetFn :: forall r a. Fn2 String (Record r) a
foreign import unsafeSetFn :: forall r1 r2 a. Fn3 String a (Record r1) (Record r2)
foreign import unsafeDeleteFn :: forall r1 r2. Fn2 String (Record r1) (Record r2)
foreign import unsafeHasFn :: forall r1. Fn2 String (Record r1) Boolean

unsafeGet :: forall r a. String -> Record r -> a
unsafeGet = runFn2 unsafeGetFn

unsafeSet :: forall r1 r2 a. String -> a -> Record r1 -> Record r2
unsafeSet = runFn3 unsafeSetFn

unsafeDelete :: forall r1 r2. String -> Record r1 -> Record r2
unsafeDelete = runFn2 unsafeDeleteFn

unsafeHas :: forall r1. String -> Record r1 -> Boolean
unsafeHas = runFn2 unsafeHasFn
