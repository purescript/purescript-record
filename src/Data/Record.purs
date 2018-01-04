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
  , class SequenceRecord
  , rsequence
  , rsequenceImpl
  ) where

import Data.Function.Uncurried (runFn2, runFn3)
import Data.Record.Unsafe (unsafeGetFn, unsafeSetFn, unsafeDeleteFn)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Prelude (class Applicative, class Eq, pure, (&&), (<$>), (<*>), (==))
import Type.Equality (class TypeEquals, to)
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

-- | A function similar to `Data.Traversable.sequence`, but operating on
-- | heterogenous records instead of homogenous containers.
-- |
-- | `rsequence` runs the actions contained in record fields, and accumulates
-- | the results in a record. The actions are sequenced in the order of sorted
-- | field names (the order in which `RowToList` returns fields).
-- |
-- | Example types that match the general type of `rsequence`:
-- |
-- | - `rsequence :: { a :: Maybe Int, b :: Maybe Int } -> Maybe { a :: Int, b :: Int }`
-- | - `rsequence :: { a :: Array Int, b :: Array Int } -> Array { a :: Int, b :: Int }`
rsequence
  :: forall f fr frs r
   . RowToList fr frs
  => SequenceRecord f frs fr r
  => Record fr
  -> f (Record r)
rsequence = rsequenceImpl (RLProxy :: RLProxy frs)

class SequenceRecord (f :: Type -> Type) (frs :: RowList) (fr :: # Type) (r :: # Type)
  | frs -> r where
  rsequenceImpl :: RLProxy frs -> Record fr -> f (Record r)

instance sequenceRecordCons
  ::
  ( IsSymbol name
  , Applicative f
  , RowCons name ty tailRow r
  , RowLacks name tailRow
  , RowCons name (f ty) trash fr
  , SequenceRecord f tail fr tailRow
  ) => SequenceRecord f (Cons name (f ty) tail) fr r where
  rsequenceImpl _ fr = insert nameP <$> value <*> rest
    where
      nameP :: SProxy name
      nameP = SProxy

      value :: f ty
      value = get (SProxy :: SProxy name) fr
      
      rest :: f (Record tailRow)
      rest = rsequenceImpl (RLProxy :: RLProxy tail) fr

instance sequenceRecordNil
  ::
  ( Applicative f
  , TypeEquals {} (Record empty)
  ) => SequenceRecord f Nil fr empty where
  rsequenceImpl _ _ = pure (to {})
