module Examples where

import Prelude

import Record as Record
import Type.Proxy (Proxy(..))

x_ = Proxy :: Proxy "x"
y_ = Proxy :: Proxy "y"
z_ = Proxy :: Proxy "z"

gotX :: Int
gotX = Record.get x_ { x: 1 }

insertedX :: { x :: Int }
insertedX = Record.insert x_ 1 {}

deletedX :: {}
deletedX = Record.delete x_ { x: 1 }

setX1 :: { x :: Int }
setX1 = Record.set x_ 1 { x: 0 }

setX2 :: { x :: Unit }
setX2 = Record.set x_ unit { x: 0 }

modifyX :: { x :: Int }
modifyX = Record.modify x_ (\value -> value + 1) { x: 0 }

mergedXY :: { x :: Int , y :: Int }
mergedXY = Record.merge { x: 1 } { y: 1 }
