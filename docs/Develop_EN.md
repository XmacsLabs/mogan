# Developers' Guide
## Developing on macOS or GNU/Linux
Assuming you are in the project root of mogan:
``` bash
mkdir build && cd build
cmake .. -DCMAKE_INSTALL_PREFIX=$HOME/software
make install -j12
```

Now, you can launch Mogan via:
``` bash
$HOME/software/bin/mogan.sh
```

Using `make install` and developing in a separated folder could help you setup a clean environment. If you are familiar with the source code, you can change the TEXMACS_PATH to develop on the Scheme part without having to build the C++ part.

## Developing for Windows
We need to install WSL Ubuntu 20.04 on Windows or install a Windows virtualbox on Ubuntu 20.04. Because building is done on Ubuntu and testing is done on Windows.

First of all, we need to download MXE and install all the related dependencies (step 1 and step 2):
https://texmacs.github.io/notes/docs/build-using-cmake-and-mxe-on-wsl.html

Assuming you are in the project root of mogan:
```
rm -rf build/
./packages/windows/package.sh
```
After the packaging, we need to share or copy the directory between the host system and the guest system, and then test Mogan in Windows.

