module Record.ST
  ( STRecord
  , freeze
  , thaw
  , peek
  , poke
  , modify
  ) where

import Prelude

import Control.Monad.ST (ST, kind Region)
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Prim.Row as Row

-- | A value of type `STRecord h r` represents a mutable record with fields `r`,
-- | belonging to the state thread `h`.
-- |
-- | Create values of type `STRecord` using `thaw`.
foreign import data STRecord :: Region -> # Type -> Type

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
  :: forall l h a r r1
   . Row.Cons l a r1 r
  => IsSymbol l
  => SProxy l
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
  :: forall l h a r r1
   . Row.Cons l a r1 r
  => IsSymbol l
  => SProxy l
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
  :: forall l h a r r1
   . Row.Cons l a r1 r
  => IsSymbol l
  => SProxy l
  -> (a -> a)
  -> STRecord h r
  -> ST h Unit
modify l = unsafeModify (reflectSymbol l)
