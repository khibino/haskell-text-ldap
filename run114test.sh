#! /bin/sh

set -x

cabal clean
cabal configure --enable-tests --flags=test-cabal114
cabal build
cabal test
