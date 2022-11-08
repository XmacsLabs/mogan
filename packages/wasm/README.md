# Mogan for Webassembly

Code taken and modified from [TeXmacs on WASM](https://github.com/texmacs/texmacs/tree/wip_wasm)
Comparing to native build some functionalities are removed, mainly system call related. Some Plugins (e.g. Mplayer,Pdf) are also removed because I cannot make them work, see CMakeLists.txt for more detail.

I've tested with Qt6.4 with it's corresponding emsdk (Emscripten 3.1.14), see instructions [here](https://doc.qt.io/qt-6/wasm.html). I use Qt Creator to setup all dependencies (cmake toolchain file etc.), see instruction [here](https://doc.qt.io/qtcreator/creator-setup-webassembly.html).

## Instruction

2. Copy the file packages/wasm/tm-server.scm to override TeXmacs/progs/texmacs/texmacs/tm-server.scm
3. Use Qt Creator to configure and build. You may also use command line tool emcmake and emmake which ship with emsdk, but I haven't tried.
4. After build, you will see following files: Mogan.data, Mogan.wasm, Mogan.js, Mogan.html, qtloader.js, qtlogo.svg. Put them in a folder and run following command to start a local server:
```shell
python -m http.server
```
Then you can open Mogan.html at localhost:8000 in Chrome or Edge. (my Firefox doesn't work)
