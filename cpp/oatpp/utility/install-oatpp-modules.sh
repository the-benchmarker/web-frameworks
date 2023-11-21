#!/bin/sh

rm -rf tmp

mkdir tmp
cd tmp

##########################################################
## install oatpp

MODULE_NAME="oatpp"

git clone --depth=1 https://github.com/oatpp/$MODULE_NAME

cd $MODULE_NAME
mkdir build
cd build

cmake -DOATPP_BUILD_TESTS=OFF -DCMAKE_BUILD_TYPE=Release ..
make install -j 6

cd ../../

##########################################################

cd ../

rm -rf tmp
