export DYLD_LIBRARY_PATH=../dist-newstyle/build/aarch64-osx/ghc-8.10.7/haskell-diagrams-cellular-automata-0.1.0.0/build:/Users/amilkowski/.ghcup/ghc/8.10.7/lib/ghc-8.10.7/rts

make

#install_name_tool -add_rpath /Users/amilkowski/go/src/blockbook/ml/ca/server/cellularAutomata/dist-newstyle/build/aarch64-osx/ghc-8.10.7/haskell-diagrams-cellular-automata-0.1.0.0/build libGenerate.so
#install_name_tool -add_rpath /Users/amilkowski/.ghcup/ghc/8.10.7/lib/ghc-8.10.7/rts libGenerate.so
