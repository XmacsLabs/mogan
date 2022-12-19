# Developing for Windows
## Using xmake on Windows
This project uses variant length array, so cannot be compiled by msvs. It is recommand to use mingw as compiler.

### Step 1: Install xmake and Qt 5
Developers should install such tools:

* [Qt](https://www.qt.io/download)
    * only qt library for mingw 8.1.0 is needed, it is ok to use install qt5 and qt6. Other qt framework like qt script is not needed.
    * recommand to install mingw 8.1.0, which can be found in tools.
* [msys environment](https://github.com/msys2/msys2-installer/releases)
* mingw 8.1.0 compiler (see below).
* xmake (see below).

Xmake can be installed either from msys pacman, or from standalone installer for windows.

**CAUTIONS**: Mingw in msys pacman is too new thus imconpatible for this project. Please install mingw 8.1.0 for this project, from either Qt installer or from chocolate.

Sometimes, we need the latest xrepo:
``` pwsh
xrepo update-repo
```

### Step 2: Compile
Run these command in msys environment.
``` bash
xmake config --qt=<newly installed qt address>/5.x.x/mingw81_64
xmake build --jobs=<numbers of processes your computer can support, same as make>
```

### Step 3: Run unit test
``` bash
windeployqt --compiler-runtime ./build/mingw/x86_64/release/ -printsupport
xmake run --yes --verbose --diagnosis --group=tests
xmake run --yes --verbose --diagnosis --group=keneral_tests
```

### Step 4: Install to `build/package`
``` bash
xmake install -o build/package mogan_install
windeployqt --compiler-runtime ./build/package/bin/ -printsupport
```

There are other binary used by mogan as optional dependencies, you can found them at [xmacslabs/mogan-dependencies](https://github.com/XmacsLabs/mogan-dependencies).

### Step 5: Launch Mogan Editor
``` bash
build/package/bin/mogan.exe
```

### Optional: Vscode support
Developers using Vscode can use xmake to generate `compile_command.json`, which can be recognized by C/C++ plugin to provide semantics highlight and so on.
```bash
xmake project --kind=compile_commands ./.vscode
```

then modify `.vscode/c_cpp_properties.json` to tell C++ lsp configs of this project:
```jsonc
{
    {
    "configurations": [
        {
            // other configs
            "compilerPath": "<your configuration>",
            "cppStandard": "gnu++17",
            "intelliSenseMode": "windows-gcc-x64",
            "compileCommands": ".vscode/compile_commands.json"
        }
    ],
}
}
```

##  Using CMake on Linux or WSL environment to cross-compile
We need to install WSL Ubuntu 20.04 on Windows or install a Windows virtualbox on Ubuntu 20.04. Because building is done on Ubuntu and testing is done on Windows.

First of all, we need to download MXE and install all the related dependencies (step 1 and step 2):
https://texmacs.github.io/notes/docs/build-using-cmake-and-mxe-on-wsl.html

Assuming you are in the project root of mogan:
```
rm -rf build/
./packages/windows/package.sh
```
After the packaging, we need to share or copy the directory between the host system and the guest system, and then test Mogan in Windows.

