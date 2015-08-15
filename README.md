# purescript-free

[![Latest release](http://img.shields.io/bower/v/purescript-free.svg)](https://github.com/purescript/purescript-free/releases)
[![Build Status](https://travis-ci.org/purescript/purescript-free.svg?branch=master)](https://travis-ci.org/purescript/purescript-free)
[![Dependency Status](https://www.versioneye.com/user/projects/55848c7336386100150003e9/badge.svg?style=flat)](https://www.versioneye.com/user/projects/55848c7336386100150003e9)

Free monad, Cofree comonad, Yoneda and Coyoneda functors, and the Trampoline monad implementations for PureScript.

The Free monad implementation is represented using a sequential data structure.

See the following reference for further information.
* [Relection without Remorse](http://okmij.org/ftp/Haskell/zseq.pdf) (Ploeg and Kiselyov 2014)

## Installation

```
bower install purescript-free
```

## Documentation

* [Control.Monad.Free](docs/Control/Monad/Free.md)
* [Control.Comonad.Cofree](docs/Control/Comonad/Cofree.md)
* [Data.Yoneda](docs/Data/Yoneda.md)
* [Data.Coyoneda](docs/Data/Coyoneda.md)
* [Control.Monad.Trampoline](docs/Control/Monad/Trampoline.md)

## Benchmarks

The following benchmarks compare the current implementation with the implementation at `v0.6.1` (0df59c5d459fed983131856886fc3a4b43234f1f), which used the `Gosub` technique to defer monadic binds.

The benchmarks may be run as follows. Note that `pulp` must be on your path.

```bash
npm install

npm run-script benchmark
```

![left-bind-small](benchmark/left-bind-small.png)

![left-bind-large](benchmark/left-bind-large.png)

![right-bind-small](benchmark/right-bind-small.png)

![right-bind-large](benchmark/right-bind-large.png)
