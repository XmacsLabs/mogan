#!/bin/bash

export PATH=$PATH:$HOME/github/mxe/mxe/usr/bin

mkdir -p build && cd build/
if [ "$1" = "win32" ];then
  i686-w64-mingw32.static-cmake .. \
    -DCMAKE_INSTALL_PREFIX=$HOME/win/Mogan \
    -DWIN_BIT_SIZE=32
else
  x86_64-w64-mingw32.static-cmake .. \
    -DCMAKE_INSTALL_PREFIX=$HOME/win/Mogan \
    -DWIN_BIT_SIZE=64
fi

make -j12 && make install -j12

