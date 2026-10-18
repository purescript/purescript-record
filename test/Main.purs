module Test.Main where

import Prelude

import Effect (Effect)
import Record (delete, equal, get, insert, merge, modify, rename, set)
import Record.Builder as Builder
import Record.Unsafe (unsafeHas)
import Test.Assert (assert')
import Type.Proxy (Proxy(..))

main :: Effect Unit
main = do
  let
    x = Proxy :: Proxy "x"
    y = Proxy :: Proxy "y"
    z = Proxy :: Proxy "z"

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
    not (equal { a: 1, b: "b", c: true } { a: 1, b: "b", c: false })
  assert' "merge" $
    equal { x: 1, y: "y" } (merge { y: "y" } { x: 1, y: 2 })
  assert' "unsafeHas1" $
    unsafeHas "a" { a: 42 }
  assert' "unsafeHas2" $
    not (unsafeHas "b" { a: 42 })

  let
    builder =
      Builder.insert x 42
        >>> Builder.merge { y: true, z: "testing" }
        >>> Builder.delete y
        >>> Builder.modify x show
        >>> Builder.rename z y
    testBuilder = Builder.build builder {}

  assert' "Record.Builder" $
    testBuilder.x == "42" && testBuilder.y == "testing"

  let
    merged =
      Builder.build (Builder.merge { x: 1, y: "y" }) { y: 2, z: true }
        :: { x :: Int, y :: String, z :: Boolean }
  assert' "Record.Builder.merge" $
    merged.x == 1 && merged.y == "y" && merged.z

  let
    unioned =
      Builder.build (Builder.union { x: 1, y: "y" }) { y: 2, z: true }
        :: { x :: Int, y :: String, y :: Int, z :: Boolean }
  assert' "Record.Builder.union" $
    unioned.x == 1 && unioned.y == "y" && unioned.z

  let
    flipMerged =
      Builder.build (Builder.flip Builder.merge { x: 1, y: "y" }) { y: 2, z: true }
        :: { x :: Int, y :: Int, z :: Boolean }
  assert' "Record.Builder.flip merge" $
    flipMerged.x == 1 && flipMerged.y == 2 && flipMerged.z

  let
    flipUnioned =
      Builder.build (Builder.flip Builder.union { x: 1, y: "y" }) { y: 2, z: true }
        :: { x :: Int, y :: Int, y :: String, z :: Boolean }
  assert' "Record.Builder.flip union" $
    flipUnioned.x == 1 && flipUnioned.y == 2 && flipUnioned.z
