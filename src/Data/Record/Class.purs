module Data.Record.Class (
    class Subrow,
    class RecordMerge,
    class IntersectRow,
    merge,
    unionMerge
) where

-- | Proof that row `r` is a subset of row `s`
class Subrow (r :: # Type) (s :: # Type)
instance srInst :: Union r t s => Subrow r s

-- | Proof of row `i` being the intersection of rows `ri` and `si`,
-- | `r` is `i` subtracted from `ri` and
-- | `s` is `i` subtracted from `si`
class IntersectRow (ri :: # Type) (si :: # Type) (i :: # Type) (r :: # Type) (s :: # Type)
instance irInst :: (Union r i ri, Union i s si) => IntersectRow ri si i r s

class RecordMerge (o :: # Type) (mr :: # Type) (mo :: # Type)
instance rmInst :: (IntersectRow mo mr m o r, Subrow r o) => RecordMerge o mr mo


-- | Merge any two records together unsafely.
-- | Fields common between the two will result in the value from r2 being kept
foreign import unsafeMerge
  :: forall r1 r2 r3
  .  Record r1
  -> Record r2
  -> Record r3

-- | Merge a record `mr` with optional default values `o`, resulting in record `mo`.
-- |
-- | The record `mr` must consist of the common fields from `mo` and `mr` plus a subset
-- | of fields from `o`.
-- |
-- | Examples:
-- | * `merge {a:1,b:"Unspecified"} {a:3,c:"Mandatory"} = {a:3,b:"Unspecified",c:"Mandatory"}`
-- | * `merge {a:1,b:"Unspecified"} {c:"Only mandatory"} = {a:1,b:"Unspecified",c:"Only Mandatory"}`
-- | * `merge {a:1,b:"Unspecified"} {a:"Wrong type"} = wont compile`
merge :: forall o mr mo. RecordMerge o mr mo => Record o -> Record mr -> Record mo
merge = unsafeMerge

-- | Merge record `a` with `b` resulting in `c`. The `Union` constraint means that `c`
-- | will contain all the fields from both `a` and `b`, with `b`'s appearing first in the
-- | list of types.
-- |
-- | Examples:
-- | * `unionMerge {a:"Default"} {a:1} = {a:1} :: {a::Int,a::String}`
-- | * `unionMerge {a:"Default",b:2} {a:1} = {a:1,b:2} :: {a::Int,a::String,b::Int}`
unionMerge :: forall a b c. Union b a c => Record a -> Record b -> Record c
unionMerge = unsafeMerge
