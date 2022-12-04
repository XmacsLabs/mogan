# Developers' Guide
## Developing for Windows using CMake
We need to install WSL Ubuntu 20.04 on Windows or install a Windows virtualbox on Ubuntu 20.04. Because building is done on Ubuntu and testing is done on Windows.

First of all, we need to download MXE and install all the related dependencies (step 1 and step 2):
https://texmacs.github.io/notes/docs/build-using-cmake-and-mxe-on-wsl.html

Assuming you are in the project root of mogan:
```
rm -rf build/
./packages/windows/package.sh
```
After the packaging, we need to share or copy the directory between the host system and the guest system, and then test Mogan in Windows.

