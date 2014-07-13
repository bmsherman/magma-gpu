magma-gpu
=========

This Haskell library provides FFI bindings for some of the functions
provided by the MAGMA GPU library. Template Haskell and language-c 
are used to automatically parse the C headers for the library and
create the proper FFI declarations.

Everything is provided in the `Foreign.CUDA.Magma` module. Some functions
provide primitive marshalling, while others don't.

The `Magma` typeclass represents elements for which MAGA operations can
be performed. Its instances are `CFloat`, `CDouble`, `Complex CFloat`, and
`Complex CDouble`. The `Magma1` typeclass involves functions that have both
real and complex types, and so it has `CFloat` and `CDouble` as instances.

### Documentation

[See the Haddock documentation](http://bmsherman.github.io/haddock/magma-gpu/).

Installation
------------

First, CUDA, MAGMA, and Autoconf should be installed.
This has been tested with CUDA versions 5.5 and 6.0.
Additionally, you may need to add some CUDA directories to your `PATH`
and `LD_LIBRARY_PATH` environment variables.

Currently, the installation process is not great! I have the
MAGMA directories hard-coded in `configure.ac`, so those may need to be
changed based on where MAGMA has been installed.

In the base directory, prepare a configure script by running
```shell
autoconf configure.ac > configure
```

Then (also in the base directory),
```shell
cabal configure
cabal install
```

