#!/usr/bin/env bash

echo "running ----" && \
stack exec ./dist-newstyle/build/aarch64-osx/ghc-8.10.7/haskell-diagrams-cellular-automata-0.1.0.0/x/haskell-diagrams-cellular-automata-exe/build/haskell-diagrams-cellular-automata-exe/haskell-diagrams-cellular-automata-exe  --   -h 256 -w 256 -o images/simple.gif
