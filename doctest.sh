#!/usr/bin/env bash
# cabal install doctest --ignore-project --overwrite-policy=always && cabal build && cabal repl --build-depends=QuickCheck --build-depends=template-haskell --with-compiler=doctest --repl-options='-w -Wdefault'
doctest src/ app/
