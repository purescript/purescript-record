# Changelog

Notable changes to this project are documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Breaking changes:

New features:

Bugfixes:

Other improvements:
- Skipped needing to copy initiial record for `buildFromScratch`. (#80)

## [v4.0.0](https://github.com/purescript/purescript-record/releases/tag/v4.0.0) - 2022-04-27

Breaking changes:
- Migrate FFI to ES modules (#81 by @kl0tl and @JordanMartinez)
- Replaced polymorphic proxies with monomorphic `Proxy` (#81 by @JordanMartinez)

New features:

Bugfixes:

Other improvements:

## [v3.0.0](https://github.com/purescript/purescript-record/releases/tag/v3.0.0) - 2021-02-26

Breaking changes:
- Added support for PureScript 0.14 and dropped support for all previous versions (#66)
- Updated `Record.Builder.merge` and `Record.Builder.union` so that they behave like `Record.merge` and `Record.union`: fields from the argument override those of the record being built in case of overlaps. (#73)
- Removed `Record.ST` (#78)

New features:
- Added `buildFromScratch` for building from an empty record (#53)
- Added `flip` function (#73)

Bugfixes:

Other improvements:
- Replaced monomorphic proxies with `Type.Proxy.Proxy` and polymorphic variants (#67)
- Removed `SProxy` from documentation (#70)
- Migrated CI to GitHub Actions and updated installation instructions to use Spago (#69)
- Added a changelog and pull request template (#74, #75)

## [v2.0.2](https://github.com/purescript/purescript-record/releases/tag/v2.0.2) - 2020-03-14

- Fixed typo in docs (@i-am-the-slime)
- Fixed travis build

## [v2.0.1](https://github.com/purescript/purescript-record/releases/tag/v2.0.1) - 2019-05-27

- Dropped typelevel-prelude dependency (@hdgarrood)

## [v2.0.0](https://github.com/purescript/purescript-record/releases/tag/v2.0.0) - 2019-03-02

- Bumped dependencies (in particular, now using v4.x of typelevel-prelude) (@justinwoo)
- Added some examples to the README (@justinwoo)
- Added comments explaining Builder (@chexxor)

## [v1.0.0](https://github.com/purescript/purescript-record/releases/tag/v1.0.0) - 2018-05-23

- Updated for PureScript 0.12
- The namespace has been changed from `Data.Record` to just `Record`
- Added `modify` to the `ST` module (@matthewleon)
- Added new functions for merging records (@natefaubion)
- The `STRecord` prefixes have been dropped from the record functions for less repetition when using qualified imports
- The function argument order has been changed so that `STRecord` is always the last argument

## [v0.2.6](https://github.com/purescript/purescript-record/releases/tag/v0.2.6) - 2018-01-28

- Added `Builder.modify` (@justinwoo)

## [v0.2.5](https://github.com/purescript/purescript-record/releases/tag/v0.2.5) - 2017-11-15

- Added `rename` and `Builder.rename` (@justinwoo)

## [v0.2.4](https://github.com/purescript/purescript-record/releases/tag/v0.2.4) - 2017-10-24

- Added `Data.Record.ST` module (@paf31)

## [v0.2.3](https://github.com/purescript/purescript-record/releases/tag/v0.2.3) - 2017-09-26

- Added `unsafeHas` (@natefaubion)

## [v0.2.2](https://github.com/purescript/purescript-record/releases/tag/v0.2.2) - 2017-09-10

- Added `equal` function (@justinwoo)

## [v0.2.1](https://github.com/purescript/purescript-record/releases/tag/v0.2.1) - 2017-09-10

- Added `ST` and `Builder` modules for modifying and building records in-place.

## [v0.2.0](https://github.com/purescript/purescript-record/releases/tag/v0.2.0) - 2017-07-24

- Added unsafe versions of functions (@natefaubion)

## [v0.1.0](https://github.com/purescript/purescript-record/releases/tag/v0.1.0) - 2017-07-19

- Initial versioned release
