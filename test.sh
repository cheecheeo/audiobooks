#!/usr/bin/env bash
set -x
# cabal repl --with-compiler=doctest
./hspec.sh && ./doctest.sh && rm -f audio_files.m4b
