#!/usr/bin/env bash
cabal repl --with-compiler=doctest --build-depends=QuickCheck --build-depends=template-haskell
