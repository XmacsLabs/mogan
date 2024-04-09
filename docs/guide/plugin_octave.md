# Octave Plugin

## Installation
The Octave plugin relies on the [Ghostscript Binary Plugin](plugin_binary_gs.md). Please install Ghostscript according to the documentation first and then check `Help -> Plugins -> Octave` to ensure that the plots in the documentation can be rendered correctly.

### Windows
Go to the [Octave official website](https://octave.org), download and install Octave 5.2.0.

For example, download the Octave 5.2.0 installation package provided by the USTC open source software mirror:
+ http://mirrors.ustc.edu.cn/gnu/octave/windows/octave-5.2.0_1-w64-installer.exe
+ http://mirrors.ustc.edu.cn/gnu/octave/windows/octave-5.2.0_1-w64-installer.exe.sig

### macOS
```
brew install octave
```

### Linux
On Debian and its derivatives, use the following command to install:
```
sudo apt install octave
```

## Tips
1. Windows: The plotting functionality of Octave versions 6.x and above does not work properly in Mogan.
   + We will work on making Mogan compatible with the new versions of Octave in the future, and contributions from the community are welcome.
2. Ubuntu: Octave installed using snap cannot be used normally in Mogan.
   + This is due to the mechanism of snap, so it is not recommended to install Octave using snap.

## Documentation: Please check `Help -> Plugins -> Octave`