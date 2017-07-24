module Data.Record
  ( get
  , set
  , modify
  , insert
  , delete
  ) where

import Data.Function.Uncurried (runFn2, runFn3)
import Data.Record.Unsafe (unsafeGetFn, unsafeSetFn, unsafeDeleteFn)
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Type.Row (class RowLacks)

-- | Get a property for a label which is specified using a value-level proxy for
-- | a type-level string.
-- |
-- | For example:
-- |
-- | ```purescript
-- | get (SProxy :: SProxy "x") :: forall r a. { x :: a | r } -> a
-- | ```
get
  :: forall r r' l a
   . IsSymbol l
  => RowCons l a r' r
  => SProxy l
  -> Record r
  -> a
get l r = runFn2 unsafeGetFn (reflectSymbol l) r

-- | Set a property for a label which is specified using a value-level proxy for
-- | a type-level string.
-- |
-- | For example:
-- |
-- | ```purescript
-- | set (SProxy :: SProxy "x")
-- |   :: forall r a b. a -> { x :: b | r } -> { x :: a | r }
-- | ```
set
  :: forall r1 r2 r l a b
   . IsSymbol l
  => RowCons l a r r1
  => RowCons l b r r2
  => SProxy l
  -> b
  -> Record r1
  -> Record r2
set l b r = runFn3 unsafeSetFn (reflectSymbol l) b r

-- | Modify a property for a label which is specified using a value-level proxy for
-- | a type-level string.
-- |
-- | For example:
-- |
-- | ```purescript
-- | modify (SProxy :: SProxy "x")
-- |   :: forall r a b. (a -> b) -> { x :: a | r } -> { x :: b | r }
-- | ```
modify
  :: forall r1 r2 r l a b
   . IsSymbol l
  => RowCons l a r r1
  => RowCons l b r r2
  => SProxy l
  -> (a -> b)
  -> Record r1
  -> Record r2
modify l f r = set l (f (get l r)) r

-- | Insert a new property for a label which is specified using a value-level proxy for
-- | a type-level string.
-- |
-- | For example:
-- |
-- | ```purescript
-- | insert (SProxy :: SProxy "x")
-- |   :: forall r a. RowLacks "x" r => a -> { | r } -> { x :: a | r }
-- | ```
insert
  :: forall r1 r2 l a
   . IsSymbol l
  => RowLacks l r1
  => RowCons l a r1 r2
  => SProxy l
  -> a
  -> Record r1
  -> Record r2
insert l a r = runFn3 unsafeSetFn (reflectSymbol l) a r

-- | Delete a property for a label which is specified using a value-level proxy for
-- | a type-level string.
-- |
-- | Note that the type of the resulting row must _lack_ the specified property.
-- | Since duplicate labels are allowed, this is checked with a type class constraint.
-- |
-- | For example:
-- |
-- | ```purescript
-- | delete (SProxy :: SProxy "x")
-- |   :: forall r a. RowLacks "x" r => { x :: a | r } -> { | r }
-- | ```
delete
  :: forall r1 r2 l a
   . IsSymbol l
  => RowLacks l r1
  => RowCons l a r1 r2
  => SProxy l
  -> Record r2
  -> Record r1
delete l r = runFn2 unsafeDeleteFn (reflectSymbol l) r
