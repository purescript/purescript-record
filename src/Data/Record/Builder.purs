module Data.Record.Builder
  ( Builder
  , build
  , insert
  , modify
  , delete
  , rename
  , merge
  ) where

import Prelude

import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Type.Row (class RowLacks)

foreign import copyRecord :: forall r1. Record r1 -> Record r1
foreign import unsafeInsert :: forall a r1 r2. String -> a -> Record r1 -> Record r2
foreign import unsafeModify :: forall a b r1 r2. String -> (a -> b) -> Record r1 -> Record r2
foreign import unsafeDelete :: forall r1 r2. String -> Record r1 -> Record r2
foreign import unsafeRename :: forall r1 r2. String -> String -> Record r1 -> Record r2
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

-- | Build by modifying an existing field.
modify
  :: forall l a b r r1 r2
   . RowCons l a r r1
  => RowCons l b r r2
  => IsSymbol l
  => SProxy l
  -> (a -> b)
  -> Builder (Record r1) (Record r2)
modify l f = Builder \r1 -> unsafeModify (reflectSymbol l) f r1

-- | Build by deleting an existing field.
delete
  :: forall l a r1 r2
   . IsSymbol l
   => RowLacks l r1
   => RowCons l a r1 r2
   => SProxy l
   -> Builder (Record r2) (Record r1)
delete l = Builder \r2 -> unsafeDelete (reflectSymbol l) r2

-- | Build by renaming an existing field.
rename :: forall l1 l2 a r1 r2 r3
   . IsSymbol l1
  => IsSymbol l2
  => RowCons l1 a r2 r1
  => RowLacks l1 r2
  => RowCons l2 a r2 r3
  => RowLacks l2 r2
  => SProxy l1
  -> SProxy l2
  -> Builder (Record r1) (Record r3)
rename l1 l2 = Builder \r1 -> unsafeRename (reflectSymbol l1) (reflectSymbol l2) r1

-- | Build by merging existing fields from another record.
merge
  :: forall r1 r2 r3
   . Union r1 r2 r3
  => Record r2
  -> Builder (Record r1) (Record r3)
merge r2 = Builder \r1 -> unsafeMerge r1 r2
