module Data.Record.Unsafe
  ( unsafeGetFn
  , unsafeSetFn
  , unsafeDeleteFn
  , unsafeHasFn
  , unsafeGet
  , unsafeSet
  , unsafeDelete
  , unsafeMerge
  , unsafeMergeFn
  , unsafeHas
  ) where

import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)

foreign import unsafeGetFn :: forall r a. Fn2 String (Record r) a
foreign import unsafeSetFn :: forall r1 r2 a. Fn3 String a (Record r1) (Record r2)
foreign import unsafeDeleteFn :: forall r1 r2. Fn2 String (Record r1) (Record r2)
foreign import unsafeMergeFn :: forall r1 r2 r3. Fn2 (Record r1) (Record r2) (Record r3)
foreign import unsafeHasFn :: forall r1. Fn2 String (Record r1) Boolean

unsafeGet :: forall r a. String -> Record r -> a
unsafeGet = runFn2 unsafeGetFn

unsafeSet :: forall r1 r2 a. String -> a -> Record r1 -> Record r2
unsafeSet = runFn3 unsafeSetFn

unsafeDelete :: forall r1 r2. String -> Record r1 -> Record r2
unsafeDelete = runFn2 unsafeDeleteFn

unsafeMerge :: forall r1 r2 r3. Record r1 -> Record r2 -> Record r3
unsafeMerge = runFn2 unsafeMergeFn

unsafeHas :: forall r1. String -> Record r1 -> Boolean
unsafeHas = runFn2 unsafeHasFn
