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
import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.Row (class Lacks, class Cons, class Nub, class Union)
import Prim.RowList (class RowToList, RowList, Cons, Nil)
import Record.Unsafe (unsafeGet, unsafeSet, unsafeDelete)
import Record.Unsafe.Union (unsafeUnionFn)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

-- | Get a property for a label which is specified using a value-level proxy for
-- | a type-level string.
-- |
-- | For example:
-- |
-- | ```purescript
-- | get (Proxy :: Proxy "x") { x: 14 }
-- | -- 14
-- | ```
-- |
-- | More generally:
-- |
-- | ```purescript
-- | get (Proxy :: Proxy "x") :: forall r a. { x :: a | r } -> a
-- | ```
get
  :: forall proxy r r' l a
   . IsSymbol l
  => Cons l a r' r
  => proxy l
  -> Record r
  -> a
get l r = unsafeGet (reflectSymbol l) r

-- | Set a property for a label which is specified using a value-level proxy for
-- | a type-level string.
-- |
-- | For example:
-- |
-- | ```purescript
-- | set (Proxy :: Proxy "x") "new fixed value" { x: "original" }
-- | -- { x: "new fixed value" }
-- | ```
-- |
-- | More generally:
-- |
-- | ```purescript
-- | set (Proxy :: Proxy "x")
-- |   :: forall r a b. a -> { x :: b | r } -> { x :: a | r }
-- | ```
set
  :: forall proxy r1 r2 r l a b
   . IsSymbol l
  => Cons l a r r1
  => Cons l b r r2
  => proxy l
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
-- | modify (Proxy :: Proxy "x") toUpper { x: "example" }
-- | -- { x: "EXAMPLE" }
-- | ```
-- |
-- | More generally:
-- |
-- | ```purescript
-- | modify (Proxy :: Proxy "x")
-- |   :: forall r a b. (a -> b) -> { x :: a | r } -> { x :: b | r }
-- | ```
modify
  :: forall proxy r1 r2 r l a b
   . IsSymbol l
  => Cons l a r r1
  => Cons l b r r2
  => proxy l
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
-- | insert (Proxy :: Proxy "x") "a new value" { y: 14 }
-- | -- { x: "a new value", y: 14 }
-- | ```
-- |
-- | More generally:
-- |
-- | ```purescript
-- | insert (Proxy :: Proxy "x")
-- |   :: forall r a. Lacks "x" r => a -> { | r } -> { x :: a | r }
-- | ```
insert
  :: forall proxy r1 r2 l a
   . IsSymbol l
  => Lacks l r1
  => Cons l a r1 r2
  => proxy l
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
-- | delete (Proxy :: Proxy "x") { x: "I will leave", y: "I stay" }
-- | -- { y: "I stay" }
-- | ```
-- |
-- | More generally:
-- |
-- | ```purescript
-- | delete (Proxy :: Proxy "x")
-- |   :: forall r a. Lacks "x" r => { x :: a | r } -> { | r }
-- | ```
delete
  :: forall proxy r1 r2 l a
   . IsSymbol l
  => Lacks l r1
  => Cons l a r1 r2
  => proxy l
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
-- | rename (Proxy :: Proxy "x") (Proxy :: Proxy "y") { x: "I get a new key", z: "I stay" }
-- | -- { y: "I get a new key", z: "I stay" }
-- | ```
-- |
-- | More generally:
-- |
-- | ```purescript
-- | rename (Proxy :: Proxy "x") (Proxy :: Proxy "y")
-- |   :: forall a r. Lacks "x" r => Lacks "y" r => { x :: a | r} -> { y :: a | r}
-- | ```
rename :: forall proxy prev next ty input inter output
   . IsSymbol prev
  => IsSymbol next
  => Cons prev ty inter input
  => Lacks prev inter
  => Cons next ty inter output
  => Lacks next inter
  => proxy prev
  -> proxy next
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
-- | -- { x: 1, y: "y", z: true }
-- | ```
-- |
-- | More generally:
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
-- | -- { x: 1, y: "y", z: true }
-- | ```
-- |
-- | More generally:
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
-- | For example:
-- | 
-- | ```purescript
-- | disjointUnion { a: 5 } { b: 7 }
-- | -- { a: 5, b: 7 }
-- | ```
-- | 
-- | However, this example does noot compile because both records share the key `a`:
-- | ```purescript
-- | disjointUnion { a: 5 } { a: 7, b: 5 }
-- | ```
-- | 
-- | In the following example, hole `?help` is inferred to have type `{ b :: Int }`:
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
equal a b = equalFields (Proxy :: Proxy rs) a b

class EqualFields (rs :: RowList Type) (row :: Row Type) | rs -> row where
  equalFields :: forall rlproxy. rlproxy rs -> Record row -> Record row -> Boolean

instance equalFieldsCons
  ::
  ( IsSymbol name
  , Eq ty
  , Cons name ty tailRow row
  , EqualFields tail row
  ) => EqualFields (Cons name ty tail) row where
  equalFields _ a b = get' a == get' b && equalRest a b
    where
      get' = get (Proxy :: Proxy name)
      equalRest = equalFields (Proxy :: Proxy tail)

instance equalFieldsNil :: EqualFields Nil row where
  equalFields _ _ _ = true
