#!/usr/bin/env bash
set -x
# cabal repl --with-compiler=doctest
./hspec.sh && ./doctest.sh && cabal run
rm -f audio_files.m4* audio_files.flac
