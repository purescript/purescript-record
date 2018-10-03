# purescript-record

[![Latest release](http://img.shields.io/bower/v/purescript-record.svg)](https://github.com/purescript/purescript-record/releases)
[![Build Status](https://travis-ci.org/purescript/purescript-record.svg?branch=master)](https://travis-ci.org/purescript/purescript-record)

Functions for working with records and polymorphic labels

## Installation

```
bower install purescript-record
```

## Examples

Given some Symbol ("type level String") Proxy (`SProxy`) and a constrained or concrete record type, you can use this library to generically modify records.

```purs
x_ = SProxy :: SProxy "x"

-- we can get a value out of a field
gotX :: Int
gotX = Record.get x_ { x: 1 }

-- we can insert a value into a record that does not have a field at that label yet
insertedX :: { x :: Int }
insertedX = Record.insert x_ 1 {}

-- we can delete a field from a record at a specific label
deletedX :: {}
deletedX = Record.delete x_ { x: 1 }

-- we can set a new value for a field
setX1 :: { x :: Int }
setX1 = Record.set x_ 1 { x: 0 }

-- we can also modify the type of the field by replacing the contents
setX2 :: { x :: Unit }
setX2 = Record.set x_ unit { x: 0 }

-- we can modify the field value with a function
modifyX :: { x :: Int }
modifyX = Record.modify x_ (\value -> value + 1) { x: 0 }

-- we can also merge two records
mergedXY :: { x :: Int , y :: Int }
mergedXY = Record.merge { x: 1 } { y: 1 }
```

See the [tests](./test/Main.purs) for more examples.

If you need to combine multiple operations and avoid intermediate values, you might consider using either [Record.Builder](https://pursuit.purescript.org/packages/purescript-record/docs/Record.Builder) or [Record.ST](https://pursuit.purescript.org/packages/purescript-record/docs/Record.ST).

You can also find an explanation and example of how to use this library [in this tutorial](https://purescript-simple-json.readthedocs.io/en/latest/inferred-record-types.html) of the Simple-JSON docs.

## Documentation

Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-record).
