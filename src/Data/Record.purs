module Data.Record
  ( get
  , set
  , modify
  , insert
  , delete
  , rename
  , equal
  , class EqualFields
  , equalFields
  ) where

import Data.Function.Uncurried (runFn2, runFn3)
import Data.Record.Unsafe (unsafeGetFn, unsafeSetFn, unsafeDeleteFn)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Prelude (class Eq, (&&), (==))
import Type.Row (class RowLacks, class RowToList, Cons, Nil, RLProxy(RLProxy), kind RowList)

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

-- | Rename a property for a label which is specified using a value-level proxy for
-- | a type-level string.
-- |
-- | Note that the type of the resulting row must _lack_ the specified property.
-- | Since duplicate labels are allowed, this is checked with a type class constraint.
-- |
-- | For example:
-- |
-- | ```purescript
-- | rename (SProxy :: SProxy "x") (SProxy :: SProxy "y")
-- |   :: forall a r. RowLacks "x" r => RowLacks "y" r => { x :: a | r} -> { y :: a | r}
-- | ```
rename :: forall prev next ty input inter output
   . IsSymbol prev
  => IsSymbol next
  => RowCons prev ty inter input
  => RowLacks prev inter
  => RowCons next ty inter output
  => RowLacks next inter
  => SProxy prev
  -> SProxy next
  -> Record input
  -> Record output
rename prev next record =
  insert next (get prev record) (delete prev record :: Record inter)

-- | Check two records of the same type for equality.
equal
  :: forall r rs
   . RowToList r rs
  => EqualFields rs r
  => Record r
  -> Record r
  -> Boolean
equal a b = equalFields (RLProxy :: RLProxy rs) a b

class EqualFields (rs :: RowList) (row :: # Type) | rs -> row where
  equalFields :: RLProxy rs -> Record row -> Record row -> Boolean

instance equalFieldsCons
  ::
  ( IsSymbol name
  , Eq ty
  , RowCons name ty tailRow row
  , EqualFields tail row
  ) => EqualFields (Cons name ty tail) row where
  equalFields _ a b = get' a == get' b && equalRest a b
    where
      get' = get (SProxy :: SProxy name)
      equalRest = equalFields (RLProxy :: RLProxy tail)

instance equalFieldsNil :: EqualFields Nil row where
  equalFields _ _ _ = true
