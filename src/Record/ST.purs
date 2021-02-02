module Record.ST
  ( STRecord
  , run
  , freeze
  , thaw
  , peek
  , poke
  , modify
  ) where

import Prelude

import Control.Monad.ST (ST, Region)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.Row as Row

-- | A value of type `STRecord h r` represents a mutable record with fields `r`,
-- | belonging to the state thread `h`.
-- |
-- | Create values of type `STRecord` using `thaw`.
foreign import data STRecord :: Region -> Row Type -> Type

type role STRecord nominal representational

-- | Freeze a mutable record, creating an immutable record. Use this function as you would use
-- | `Control.Monad.ST.run` (from the `purescript-st` package) to freeze a mutable reference.
-- |
-- | The rank-2 type prevents the record from escaping the scope of `run`.
foreign import run :: forall r. (forall h. ST h (STRecord h r)) -> Record r

-- | Freeze a mutable record, creating a copy.
foreign import freeze :: forall h r. STRecord h r -> ST h (Record r)

-- | Thaw an immutable record, creating a copy.
foreign import thaw :: forall h r. Record r -> ST h (STRecord h r)

foreign import unsafePeek
  :: forall a r h
   . String
  -> STRecord h r
  -> ST h a

-- | Read the current value of a field in a mutable record, by providing a
-- | type-level representative for the label which should be read.
peek
  :: forall proxy l h a r r1
   . Row.Cons l a r1 r
  => IsSymbol l
  => proxy l
  -> STRecord h r
  -> ST h a
peek l = unsafePeek (reflectSymbol l)

foreign import unsafePoke
  :: forall a r h
   . String
  -> a
  -> STRecord h r
  -> ST h Unit

-- | Modify a record in place, by providing a type-level representative for the label
-- | which should be updated.
poke
  :: forall proxy l h a r r1
   . Row.Cons l a r1 r
  => IsSymbol l
  => proxy l
  -> a
  -> STRecord h r
  -> ST h Unit
poke l = unsafePoke (reflectSymbol l)

foreign import unsafeModify
  :: forall a r h
   . String
  -> (a -> a)
  -> STRecord h r
  -> ST h Unit

-- | Modify a record in place,
-- | by providing a type-level representative for the label to update
-- | and a function to update it.
modify
  :: forall proxy l h a r r1
   . Row.Cons l a r1 r
  => IsSymbol l
  => proxy l
  -> (a -> a)
  -> STRecord h r
  -> ST h Unit
modify l = unsafeModify (reflectSymbol l)
