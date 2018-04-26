module Test.Main where

import Prelude

import Effect (Effect)
import Data.Record (delete, equal, get, insert, modify, rename, set)
import Data.Record.Builder as Builder
import Control.Monad.ST (run) as ST
import Data.Record.ST (poke, thaw, freeze) as ST
import Data.Record.Unsafe (unsafeHas)
import Data.Symbol (SProxy(..))
import Test.Assert (assert')

main :: Effect Unit
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

  let stTest1 = ST.run do
        rec <- ST.thaw { x: 41, y: "" }
        ST.poke x 42 rec
        ST.poke y "testing" rec
        ST.freeze rec

  assert' "pokeSTRecord" $
    stTest1.x == 42 && stTest1.y == "testing"

  let testBuilder = Builder.build (Builder.insert x 42
                                  >>> Builder.merge { y: true, z: "testing" }
                                  >>> Builder.delete y
                                  >>> Builder.modify x show
                                  >>> Builder.rename z y) {}

  assert' "Data.Record.Builder" $
    testBuilder.x == "42" && testBuilder.y == "testing"
