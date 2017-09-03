module Data.Record.Builder
  ( Builder
  , build
  , insert
  , delete
  , merge
  ) where

import Prelude

import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Type.Row (class RowLacks)

foreign import copyRecord :: forall r1. Record r1 -> Record r1
foreign import unsafeInsert :: forall a r1 r2. String -> a -> Record r1 -> Record r2
foreign import unsafeDelete :: forall r1 r2. String -> Record r1 -> Record r2
foreign import unsafeMerge :: forall r1 r2 r3. Record r1 -> Record r2 -> Record r3

-- | A `Builder` can be used to `build` a record by incrementally adding
-- | fields in-place, instead of using `insert` and repeatedly generating new
-- | immutable records which need to be garbage collected.
-- |
-- | The `Category` instance for `Builder` can be used to compose builders.
-- |
-- | For example:
-- |
-- | ```purescript
-- | build (insert x 42 >>> insert y "testing") {} :: { x :: Int, y :: String }
-- | ```
newtype Builder a b = Builder (a -> b)

-- | Build a record, starting from some other record.
build :: forall r1 r2. Builder (Record r1) (Record r2) -> Record r1 -> Record r2
build (Builder b) r1 = b (copyRecord r1)

derive newtype instance semigroupoidBuilder :: Semigroupoid Builder
derive newtype instance categoryBuilder :: Category Builder

-- | Build by inserting a new field.
insert
  :: forall l a r1 r2
   . RowCons l a r1 r2
  => RowLacks l r1
  => IsSymbol l
  => SProxy l
  -> a
  -> Builder (Record r1) (Record r2)
insert l a = Builder \r1 -> unsafeInsert (reflectSymbol l) a r1

-- | Build by deleting an existing field.
delete
  :: forall l a r1 r2
   . IsSymbol l
   => RowLacks l r1
   => RowCons l a r1 r2
   => SProxy l
   -> Builder (Record r2) (Record r1)
delete l = Builder \r2 -> unsafeDelete (reflectSymbol l) r2

-- | Build by merging existing fields from another record.
merge
  :: forall r1 r2 r3
   . Union r1 r2 r3
  => Record r2
  -> Builder (Record r1) (Record r3)
merge r2 = Builder \r1 -> unsafeMerge r1 r2
