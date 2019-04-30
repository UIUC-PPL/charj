#!/bin/bash

# Set this variable to working C++11 compiler
export CXX=clang++
# Set this path to the charm on branch xi-builder
export XI_BUILDER_HOME=/shared/charj/xi-builder/
# Set this path to working charm
export CHARM_HOME=/shared/charj/charm-v6.8.2

for i in tests/sequential/*cp
do
    ./bin/charj ./$i -o=output --sequential
done

for i in tests/parallel/*cp
do
    ./bin/charj ./$i -o=output
done

for i in tests/msa/*cp
do
    ./bin/charj ./$i -o=output --msa
done
