#!/usr/bin/env bash

echo "running ----" && \
stack exec ./dist-newstyle/build/aarch64-osx/ghc-8.10.7/haskell-diagrams-cellular-automata-0.1.0.0/x/haskell-diagrams-cellular-automata-exe/build/haskell-diagrams-cellular-automata-exe/haskell-diagrams-cellular-automata-exe  --   -h 32 -w 1024 -o images/simple.gif
