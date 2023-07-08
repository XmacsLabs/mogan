# Developing for Windows
## Using xmake on Windows
This project uses variant length array, so cannot be compiled by msvs. It is recommended to use MinGW as compiler.

### Step 1: Install xmake and Qt 5
Developers should install such tools:

* [Qt](https://www.qt.io/download)
    * only Qt library for MinGW 8.1.0 is needed, it is ok to use Qt 5 or Qt 6 (Qt 5.15.2 is a good choice and can be adapted to MinGW 8.1.0). Other Qt framework like Qt script is not needed.
* [msys environment](https://github.com/msys2/msys2-installer/releases)
* MinGW 8.1.0 compiler (see below).
    * It is recommended to install MinGW 8.1.0, which can be found in Qt Tools.
* xmake (see below).

Xmake can be installed either from msys pacman, or from standalone installer for windows.

It is recommended to use pacman, and other packages need to be installed in this way

```
pacman -Sy xmake
pacman -Sy make
pacman -Sy git
pacman -Sy mingw-w64-x86_64-7zip
```

**CAUTIONS**: MinGW in msys pacman is too new thus incompatible for this project. Please install MinGW 8.1.0 for this project, from either Qt installer or from chocolate.

Sometimes, we need the latest xrepo:
``` pwsh
xrepo update-repo
```

### Step 2: Compile
Run these command in msys environment.

```
xmake config --yes --verbose --diagnosis --plat=mingw --mingw=<newly installed qt address>/Tools/mingw810_64 --qt=<newly installed qt address>/5.x.x/mingw81_64
```

The above example is using MinGW 8.1.0 installed by the Qt installer

``` bash
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
xmake install -o build/package mogan
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
