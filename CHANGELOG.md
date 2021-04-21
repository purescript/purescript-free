# Changelog

Notable changes to this project are documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Breaking changes:

New features:

Bugfixes:

Other improvements:

## [v6.0.1](https://github.com/purescript/purescript-free/releases/tag/v6.0.1) - 2021-04-20

Bugfixes:
- Removed unused constraint from `Ord1` instance (#114 by @rintcius)

Other improvements:
- Fixed warnings revealed by v0.14.1 PS release (#115 by @JordanMartinez)

## [v6.0.0](https://github.com/purescript/purescript-free/releases/tag/v6.0.0) - 2021-02-26

Breaking changes:
- Added support for PureScript 0.14 and dropped support for all previous versions (#106, #110)

Other improvements:
- Migrated CI to GitHub Actions and updated installation instructions to use Spago (#108)
- Added a CHANGELOG.md file and pull request template (#111, #112)
- Updated benchmarks (#103)

## [v5.2.0](https://github.com/purescript/purescript-free/releases/tag/v5.2.0) - 2019-04-01

- Added `Semigroup` and `Monoid` instances for `Free` (@safareli)

## [v5.1.0](https://github.com/purescript/purescript-free/releases/tag/v5.1.0) - 2018-06-14

- Added `MonadFree` and `ComonadCofree` classes.

## [v5.0.0](https://github.com/purescript/purescript-free/releases/tag/v5.0.0) - 2018-05-23

- Updated for PureScript 0.12
- `Trampoline` is now based on `(->) Unit` rather than `Lazy` to avoid unnecessary caching

## [v4.3.0](https://github.com/purescript/purescript-free/releases/tag/v4.3.0) - 2018-04-07

- Added `wrap` for `Free` (@ajnsit)
- Added more instances for `Coyoneda`

## [v4.2.0](https://github.com/purescript/purescript-free/releases/tag/v4.2.0) - 2017-12-11

- Made `Cofree` lazier (@natefaubion)
- Added `buildCofree` and deprecate `unfoldCofree` (@natefaubion)
- Added `deferCofree` (@natefaubion)

## [v4.1.0](https://github.com/purescript/purescript-free/releases/tag/v4.1.0) - 2017-06-23

- Added `resume'` (@natefaubion)

## [v4.0.1](https://github.com/purescript/purescript-free/releases/tag/v4.0.1) - 2017-05-28

- Added `Coyoneda` docs (@coot)

## [v4.0.0](https://github.com/purescript/purescript-free/releases/tag/v4.0.0) - 2017-03-27

- Updated for PureScript 0.11
- Added `Eq1` and `Ord1` instances
- Rearrange the arguments of `unfoldCofree` to be more natural
- The `Applicative` instance is now consistent with `Monad`

## [v3.5.1](https://github.com/purescript/purescript-free/releases/tag/v3.5.1) - 2017-03-16

- Export `exploreM`.

## [v3.5.0](https://github.com/purescript/purescript-free/releases/tag/v3.5.0) - 2017-03-16

- Added `exploreM` (@coot)

## [v3.4.0](https://github.com/purescript/purescript-free/releases/tag/v3.4.0) - 2017-01-31

- Added `unCoyoneda`

## [v3.3.0](https://github.com/purescript/purescript-free/releases/tag/v3.3.0) - 2017-01-02

- Added `Eq` and `Ord` instances for `Free` and `Cofree`

## [v3.2.0](https://github.com/purescript/purescript-free/releases/tag/v3.2.0) - 2016-12-29

- Added `Foldable` and `Traversable` instances for `Free`

## [v3.1.0](https://github.com/purescript/purescript-free/releases/tag/v3.1.0) - 2016-12-24

- Added `explore` combinator for `Cofree` (@paf31)

## [v3.0.1](https://github.com/purescript/purescript-free/releases/tag/v3.0.1) - 2016-11-14

- Fixed shadowed name warnings

## [v3.0.0](https://github.com/purescript/purescript-free/releases/tag/v3.0.0) - 2016-10-16

- Updated catenable-lists dependency

## [v2.0.0](https://github.com/purescript/purescript-free/releases/tag/v2.0.0) - 2016-10-13

- Updated dependencies
- Added `unfoldCofree`

## [v1.4.0](https://github.com/purescript/purescript-free/releases/tag/v1.4.0) - 2016-09-27

- Added `hoistCofree`

## [v1.3.0](https://github.com/purescript/purescript-free/releases/tag/v1.3.0) - 2016-09-14

- Added `(:<)` operator for `Cofree` constructor

## [v1.2.0](https://github.com/purescript/purescript-free/releases/tag/v1.2.0) - 2016-07-28

- Added `substFree` for better performance when folding into another `Free` (@natefaubion)
- Implemented `hoistFree` in terms of `substFree` for better performance (@natefaubion)

## [v1.1.0](https://github.com/purescript/purescript-free/releases/tag/v1.1.0) - 2016-07-26

- Restored a `resume` function (@natefaubion)
- Improved performance of `runFree` (@natefaubion)

## [v1.0.0](https://github.com/purescript/purescript-free/releases/tag/v1.0.0) - 2016-06-01

This release is intended for the PureScript 0.9.1 compiler and newer.

**Note**: The v1.0.0 tag is not meant to indicate the library is “finished”, the core libraries are all being bumped to this for the 0.9 compiler release so as to use semver more correctly.

## [v1.0.0-rc.3](https://github.com/purescript/purescript-free/releases/tag/v1.0.0-rc.3) - 2016-05-26

- Updated for introduction of `Data.NaturalTransformation` to the prelude
- Renamed `mapF` to `hoistFree`
- Renamed `liftCoyonedaT` to `hoistCoyoneda`
- Added `hoistYoneda`

## [v1.0.0-rc.2](https://github.com/purescript/purescript-free/releases/tag/v1.0.0-rc.2) - 2016-05-22

- Relaxed constraints for some instances of `Cofree` and fixed `pure` implementation (@parsonsmatt)

## [v1.0.0-rc.1](https://github.com/purescript/purescript-free/releases/tag/v1.0.0-rc.1) - 2016-03-28

- Release candidate for the psc 0.8+ core libraries

## [v0.9.1](https://github.com/purescript/purescript-free/releases/tag/v0.9.1) - 2015-11-19

- Fixed warnings raised in psc 0.7.6

## [v0.9.0](https://github.com/purescript/purescript-free/releases/tag/v0.9.0) - 2015-09-16

- Bump `transformers` dependency

## [v0.8.0](https://github.com/purescript/purescript-free/releases/tag/v0.8.0) - 2015-08-26

Bump `transformers` to `0.7.1`.

This release requires the 0.7.4.1 release of the PureScript compiler. Previous versions of this library will _not_ work with `psc` versions < 0.7.4.1.

## [v0.7.0](https://github.com/purescript/purescript-free/releases/tag/v0.7.0) - 2015-08-16

- New implementation based on “[Reflection without Remorse](http://okmij.org/ftp/Haskell/zseq.pdf)” (@ethul)

## [v0.6.1](https://github.com/purescript/purescript-free/releases/tag/v0.6.1) - 2015-07-29

- Added `MonadRec` instance (@LiamGoodacre)

## [v0.6.0](https://github.com/purescript/purescript-free/releases/tag/v0.6.0) - 2015-07-24

- Added `injF` and renamed `injC` to `injFC` for consistency with the other `F`/`FC` functions

## [v0.5.1](https://github.com/purescript/purescript-free/releases/tag/v0.5.1) - 2015-07-23

- Added additional function variations `liftFI`, `liftFCI` and `mapFC`
- Added new functions `bindF` and `bindFC`

## [v0.5.0](https://github.com/purescript/purescript-free/releases/tag/v0.5.0) - 2015-06-30

This release works with versions 0.7.\* of the PureScript compiler. It will not work with older versions. If you are using an older version, you should require an older, compatible version of this library.

## [v0.5.0-rc.1](https://github.com/purescript/purescript-free/releases/tag/v0.5.0-rc.1) - 2015-06-07

- Initial release candidate of the library intended for the 0.7 compiler.

## [v0.4.2](https://github.com/purescript/purescript-free/releases/tag/v0.4.2) - 2015-05-13

- Update to prevent stack overflow with left-associated binds (@ethul)

## [v0.4.1](https://github.com/purescript/purescript-free/releases/tag/v0.4.1) - 2015-03-19

- Updateed docs

## [v0.4.0](https://github.com/purescript/purescript-free/releases/tag/v0.4.0) - 2015-03-02

- Renamed `go`, `goM`, etc. to `runFree`, `runFreeM` etc.
- Removed `iterM`, `goEff` and `goEffC`.
- Updated `goM` to work in any target monad which is an instance of `MonadRec`, which solves the issue of `goM` causing a stack overflow for large inputs.

## [v0.3.0](https://github.com/purescript/purescript-free/releases/tag/v0.3.0) - 2015-02-21

**This release requires PureScript v0.6.8 or later**
- Updated dependencies

## [v0.2.0](https://github.com/purescript/purescript-free/releases/tag/v0.2.0) - 2015-01-10

- Updated dependencies (@garyb)

## [v0.1.6](https://github.com/purescript/purescript-free/releases/tag/v0.1.6) - 2014-12-02

- The `FreeC` type synonym is now partially applied (@ethul)

## [v0.1.5](https://github.com/purescript/purescript-free/releases/tag/v0.1.5) - 2014-11-30

- Added `injC` and `mapF` for free monad composition (@ethul)

## [v0.1.4](https://github.com/purescript/purescript-free/releases/tag/v0.1.4) - 2014-11-18

- Fixes for extracted `Identity`

## [v0.1.3](https://github.com/purescript/purescript-free/releases/tag/v0.1.3) - 2014-10-08

- Stopgap fix, `Cofree` constructor is exported as a workaround for purescript/purescript#618 (@jdegoes)

## [v0.1.2](https://github.com/purescript/purescript-free/releases/tag/v0.1.2) - 2014-10-07

- Remove bogus `mkCofree'` constructor (@jdegoes)
- Remove unnecessary type variable from `Trampoline` type synonym (@jdegoes)

## [0.1.1](https://github.com/purescript/purescript-free/releases/tag/0.1.1) - 2014-10-01

- Updated to compatible transformers version number

## [0.1.0](https://github.com/purescript/purescript-free/releases/tag/0.1.0) - 2014-09-03

- Relocated `Free` and `Trampoline` back to `purescript-free`, and adding `Yoneda` and `Coyoneda`.

## [0.0.8](https://github.com/purescript/purescript-free/releases/tag/0.0.8) - 2014-06-01

- Specialized of `go` for `Eff` as discussed in issue #6

## [0.0.7](https://github.com/purescript/purescript-free/releases/tag/0.0.7) - 2014-05-15

- Trampolined version of `Free` (@puffnfresh)

## [0.0.6](https://github.com/purescript/purescript-free/releases/tag/0.0.6) - 2014-04-28

- Bump dependencies

## [0.0.5](https://github.com/purescript/purescript-free/releases/tag/0.0.5) - 2014-04-28

- Bump dependencies

## [0.0.4](https://github.com/purescript/purescript-free/releases/tag/0.0.4) - 2014-04-08

- Updated classes for PureScript 0.5

## [0.0.1](https://github.com/purescript/purescript-free/releases/tag/0.0.1) - 2014-04-01

- Initial release
