module Data.Record.ST
  ( STRecord
  , freezeSTRecord
  , thawSTRecord
  , peekSTRecord
  , pokeSTRecord
  , runSTRecord
  , pureSTRecord
  ) where

import Prelude

import Control.Monad.Eff (Eff, runPure)
import Control.Monad.ST (ST)
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)

-- | A value of type `STRecord h r` represents a mutable record with fields `r`,
-- | belonging to the state thread `h`.
-- |
-- | Create values of type `STRecord` using `thawSTRecord`.
foreign import data STRecord :: Type -> # Type -> Type

-- | Freeze a mutable record, creating a copy.
foreign import freezeSTRecord :: forall h r eff. STRecord h r -> Eff (st :: ST h | eff) (Record r)

-- | Thaw an immutable record, creating a copy.
foreign import thawSTRecord :: forall h r eff. Record r -> Eff (st :: ST h | eff) (STRecord h r)

-- | Run an ST computation safely, constructing a record.
foreign import runSTRecord :: forall r eff. (forall h. Eff (st :: ST h | eff) (STRecord h r)) -> Eff eff (Record r)

-- | Run an ST computation safely, constructing a record, assuming no other
-- | types of effects.
pureSTRecord :: forall r. (forall h eff. Eff (st :: ST h | eff) (STRecord h r)) -> Record r
pureSTRecord st = runPure (runSTRecord st)

foreign import unsafePeekSTRecord
  :: forall a r h eff
   . String
  -> STRecord h r
  -> Eff (st :: ST h | eff) a

-- | Read the current value of a field in a mutable record, by providing a
-- | type-level representative for the label which should be read.
peekSTRecord
  :: forall l h a r r1 eff
   . RowCons l a r1 r
  => IsSymbol l
  => SProxy l
  -> STRecord h r
  -> Eff (st :: ST h | eff) a
peekSTRecord l = unsafePeekSTRecord (reflectSymbol l)

foreign import unsafePokeSTRecord
  :: forall a r h eff
   . String
  -> a
  -> STRecord h r
  -> Eff (st :: ST h | eff) Unit

-- | Modify a record in place, by providing a type-level representative for the label
-- | which should be updated.
pokeSTRecord
  :: forall l h a r r1 eff
   . RowCons l a r1 r
  => IsSymbol l
  => SProxy l
  -> a
  -> STRecord h r
  -> Eff (st :: ST h | eff) Unit
pokeSTRecord l = unsafePokeSTRecord (reflectSymbol l)
