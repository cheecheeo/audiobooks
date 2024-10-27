#!/usr/bin/env bash
# cabal repl --with-compiler=doctest
./hspec.sh && ./doctest.sh && cabal run && rm -f audio_files.m4b
