module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.ST (pureST)
import Data.Record (delete, equal, get, insert, modify, rename, set)
import Data.Record.Builder as Builder
import Data.Record.ST (pokeSTRecord, pureSTRecord, thawSTRecord, unsafeFreeze, unsafeThaw)
import Data.Record.Unsafe (unsafeHas)
import Data.Symbol (SProxy(..))
import Test.Assert (ASSERT, assert')

main :: Eff (assert :: ASSERT) Unit
main = do
  let x = SProxy :: SProxy "x"
      y = SProxy :: SProxy "y"
      z = SProxy :: SProxy "z"

  assert' "insert, get" $
    get x (insert x 42 {}) == 42
  assert' "insert, modify, get" $
    get x (modify x (_ + 1) (insert x 42 {})) == 43
  assert' "set, get" $
    get x (set x 0 { x: 42 }) == 0
  assert' "set, modify, get" $
    get x (modify x (_ + 1) (set x 0 { x: 42 })) == 1
  assert' "delete, get" $
    get x (delete y { x: 42, y: 1337 }) == 42
  assert' "rename" $
    get y (rename x y { x: 42 }) == 42
  assert' "equal" $
    equal { a: 1, b: "b", c: true } { a: 1, b: "b", c: true }
  assert' "equal2" $
    not $ equal { a: 1, b: "b", c: true } { a: 1, b: "b", c: false }
  assert' "unsafeHas1" $
    unsafeHas "a" { a: 42 }
  assert' "unsafeHas2" $
    not $ unsafeHas "b" { a: 42 }

  let stTest1 = pureSTRecord do
        rec <- thawSTRecord { x: 41, y: "" }
        pokeSTRecord x 42 rec
        pokeSTRecord y "testing" rec
        pure rec

  let stTest2 = pureST do
        rec <- unsafeThaw { x: 41, y: "" }
        pokeSTRecord x 42 rec
        pokeSTRecord y "testing" rec
        unsafeFreeze rec

  assert' "pokeSTRecord" $
    stTest1.x == 42 && stTest1.y == "testing"
    && stTest2.x == 42 && stTest2.y == "testing"

  let testBuilder = Builder.build (Builder.insert x 42
                                  >>> Builder.merge { y: true, z: "testing" }
                                  >>> Builder.delete y
                                  >>> Builder.rename z y) {}

  assert' "Data.Record.Builder" $
    testBuilder.x == 42 && testBuilder.y == "testing"
