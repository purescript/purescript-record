# Changelog

Notable changes to this project are documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Breaking changes:

New features:

Bugfixes:

Other improvements:

## [v2.0.2](https://github.com/purescript/purescript-record/releases/tag/v2.0.2) - 2020-03-14

* Fix typo in docs (@i-am-the-slime)
* Fix travis

## [v2.0.1](https://github.com/purescript/purescript-record/releases/tag/v2.0.1) - 2019-05-27

* Drop typelevel-prelude dependency (@hdgarrood)

## [v2.0.0](https://github.com/purescript/purescript-record/releases/tag/v2.0.0) - 2019-03-02

* Bump dependencies (in particular, now using v4.x of typelevel-prelude) (@justinwoo)
* Add some examples to the README (@justinwoo)
* Add comments explaining Builder (@chexxor)

## [v1.0.0](https://github.com/purescript/purescript-record/releases/tag/v1.0.0) - 2018-05-23

- Updated for PureScript 0.12
- The namespace has been changed from `Data.Record` to just `Record`
- Added `modify` to the `ST` module (@matthewleon)
- Added new functions for merging records (@natefaubion)
- The `STRecord` prefixes have been dropped from the record functions for less repetition when using qualified imports
- The function argument order has been changed so that `STRecord` is always the last argument

## [v0.2.6](https://github.com/purescript/purescript-record/releases/tag/v0.2.6) - 2018-01-28

Add `Builder.modify` (@justinwoo)

## [v0.2.5](https://github.com/purescript/purescript-record/releases/tag/v0.2.5) - 2017-11-15

Add `rename` and `Builder.rename` (@justinwoo)

## [v0.2.4](https://github.com/purescript/purescript-record/releases/tag/v0.2.4) - 2017-10-24

Add `Data.Record.ST` module (@paf31)

## [v0.2.3](https://github.com/purescript/purescript-record/releases/tag/v0.2.3) - 2017-09-26

Add `unsafeHas` (@natefaubion)

## [v0.2.2](https://github.com/purescript/purescript-record/releases/tag/v0.2.2) - 2017-09-10

Add `equal` function (@justinwoo)

## [v0.2.1](https://github.com/purescript/purescript-record/releases/tag/v0.2.1) - 2017-09-10

Add `ST` and `Builder` modules for modifying and building records in-place.

## [v0.2.0](https://github.com/purescript/purescript-record/releases/tag/v0.2.0) - 2017-07-24

Add unsafe versions of functions (@natefaubion)

## [v0.1.0](https://github.com/purescript/purescript-record/releases/tag/v0.1.0) - 2017-07-19

Initial versioned release

