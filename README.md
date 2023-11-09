# README

Benchmarks from PLDI 2019 "Lazy Counterfactual Symbolic Execution" + scripts to preprocess the files so they can be run with current LH.

```.sh
# Step 1: Generate the transformed files in `data/unsafe_dedup/`
$ cabal run

# Step 2: Go into LH directory
$ cd path/to/liquidhaskell

# Step 3: Run LH on all files in `data/unsafe_dedup/`
path/to/lh-pldi19-benchmarks/run_lh.sh path/to/lh-pldi19-benchmarks/data/unsafe_dedup
```

To run a single file

```sh
$ cd path/to/liquidhaskell
$ stack exec ghc -- -fplugin=LiquidHaskell ~/research/lh-pldi19-benchmarks/data/unsafe_dedup/List.lhs-2015-03-19T04.43.04.lhs -i=/Users/rjhala/research/lh-pldi19-benchmarks/data/lib
```

1501 files