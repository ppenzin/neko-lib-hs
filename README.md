neko-lib
========

[![Build Status](https://travis-ci.org/ppenzin/neko-lib-hs.svg?branch=master)](https://travis-ci.org/ppenzin/neko-lib-hs)

Neko VM code generation and disassembly library for Haskell.

## Overview

[Neko VM](nekovm.org) is a minimalistic general-purpose virtual machine. This
library attempts to produce and disassemble Neko code from pure Haskell.

## Prerequisites

- Haskell compiler
- Cabal

(Haskell platform would suffice)

## Building

This project pulls down dependencies, so it is recommended to use a version of
Cabal that can do sandboxes:

```
$ cabal sandbox init
$ cabal install --only-dependencies --enable-tests
```
Full build cycle:

```
$ cabal configure --enable-tests && cabal build && cabal test
```

Configure is only needed for the first build or after changing .cabal file. To
enable tests that use Neko to run the bytecode add --neko option to the test
executable:

```
$ cabal test --test-option=--neko
```

That would pick `neko` executable from the system path. For more control one
can specify path to neko executable:

```
$ cabal test --test-options="--neko --neko-exe /path/to/neko/exe"
```

## Documentation

To generate html documentation via Cabal/Haddock:

```
$ cabal haddock
```

## Contributing

Pull requests (whether it is code or not) will be appreciated. If implementing
new functionality please add a test unless there is one already.

