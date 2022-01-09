#!/bin/bash

export PATH=$PATH:$HOME/github/mxe/mxe/usr/bin

mkdir -p build && cd build/
x86_64-w64-mingw32.static-cmake .. -DCMAKE_INSTALL_PREFIX=$HOME/win/Mogan
make -j12 && make install -j12

