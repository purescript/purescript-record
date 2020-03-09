module Record
  ( get
  , set
  , modify
  , insert
  , delete
  , rename
  , equal
  , merge
  , union
  , disjointUnion
  , nub
  , class EqualFields
  , equalFields
  ) where

import Prelude

import Data.Function.Uncurried (runFn2)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Prim.Row (class Lacks, class Cons, class Nub, class Union)
import Prim.RowList (class RowToList, kind RowList, Cons, Nil)
import Record.Unsafe (unsafeGet, unsafeSet, unsafeDelete)
import Record.Unsafe.Union (unsafeUnionFn)
import Type.Data.RowList (RLProxy(..))
import Unsafe.Coerce (unsafeCoerce)

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
  => Cons l a r' r
  => SProxy l
  -> Record r
  -> a
get l r = unsafeGet (reflectSymbol l) r

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
  => Cons l a r r1
  => Cons l b r r2
  => SProxy l
  -> b
  -> Record r1
  -> Record r2
set l b r = unsafeSet (reflectSymbol l) b r

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
  => Cons l a r r1
  => Cons l b r r2
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
-- |   :: forall r a. Lacks "x" r => a -> { | r } -> { x :: a | r }
-- | ```
insert
  :: forall r1 r2 l a
   . IsSymbol l
  => Lacks l r1
  => Cons l a r1 r2
  => SProxy l
  -> a
  -> Record r1
  -> Record r2
insert l a r = unsafeSet (reflectSymbol l) a r

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
-- |   :: forall r a. Lacks "x" r => { x :: a | r } -> { | r }
-- | ```
delete
  :: forall r1 r2 l a
   . IsSymbol l
  => Lacks l r1
  => Cons l a r1 r2
  => SProxy l
  -> Record r2
  -> Record r1
delete l r = unsafeDelete (reflectSymbol l) r

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
-- |   :: forall a r. Lacks "x" r => Lacks "y" r => { x :: a | r} -> { y :: a | r}
-- | ```
rename :: forall prev next ty input inter output
   . IsSymbol prev
  => IsSymbol next
  => Cons prev ty inter input
  => Lacks prev inter
  => Cons next ty inter output
  => Lacks next inter
  => SProxy prev
  -> SProxy next
  -> Record input
  -> Record output
rename prev next record =
  insert next (get prev record) (delete prev record :: Record inter)

-- | Merges two records with the first record's labels taking precedence in the
-- | case of overlaps.
-- |
-- | For example:
-- |
-- | ```purescript
-- | merge { x: 1, y: "y" } { y: 2, z: true }
-- |  :: { x :: Int, y :: String, z :: Boolean }
-- | ```
merge
  :: forall r1 r2 r3 r4
   . Union r1 r2 r3
  => Nub r3 r4
  => Record r1
  -> Record r2
  -> Record r4
merge l r = runFn2 unsafeUnionFn l r

-- | Merges two records with the first record's labels taking precedence in the
-- | case of overlaps. Unlike `merge`, this does not remove duplicate labels
-- | from the resulting record type. This can result in better inference for
-- | some pipelines, deferring the need for a `Nub` constraint.
-- |
-- | For example:
-- |
-- | ```purescript
-- | union { x: 1, y: "y" } { y: 2, z: true }
-- |  :: { x :: Int, y :: String, y :: Int, z :: Boolean }
-- | ```
union
  :: forall r1 r2 r3
   . Union r1 r2 r3
  => Record r1
  -> Record r2
  -> Record r3
union l r = runFn2 unsafeUnionFn l r

-- | Merges two records where no labels overlap. This restriction exhibits
-- | better inference than `merge` when the resulting record type is known,
-- | but one argument is not.
-- |
-- | For example, hole `?help` is inferred to have type `{ b :: Int }` here:
-- |
-- | ```purescript
-- | disjointUnion { a: 5 } ?help :: { a :: Int, b :: Int }
-- | ```
disjointUnion
  :: forall r1 r2 r3
   . Union r1 r2 r3
  => Nub r3 r3
  => Record r1
  -> Record r2
  -> Record r3
disjointUnion l r = runFn2 unsafeUnionFn l r

-- | A coercion which removes duplicate labels from a record's type.
nub
  :: forall r1 r2
   . Nub r1 r2
  => Record r1
  -> Record r2
nub = unsafeCoerce

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
  , Cons name ty tailRow row
  , EqualFields tail row
  ) => EqualFields (Cons name ty tail) row where
  equalFields _ a b = get' a == get' b && equalRest a b
    where
      get' = get (SProxy :: SProxy name)
      equalRest = equalFields (RLProxy :: RLProxy tail)

instance equalFieldsNil :: EqualFields Nil row where
  equalFields _ _ _ = true
