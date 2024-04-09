# Ghostscript Binary Plugin
+ Binary ID：`gs`
+ Plugin Source：
  - [Codeberg](https://codeberg.org/XmacsLabs/mogan/src/branch/branch-1.2/TeXmacs/plugins/binary/progs/binary/gs.scm)
  - [Gitee](https://gitee.com/XmacsLabs/mogan/blob/branch-1.2/TeXmacs/plugins/binary/progs/binary/gs.scm)
  - [Github](https://github.com/XmacsLabs/mogan/blob/branch-1.2/TeXmacs/plugins/binary/progs/binary/gs.scm)
+ Functionality:
  - Convert PS/PDF vector graphics to PDF bitmaps for rendering in Mogan
  - Convert PS vector graphics to PDF vector graphics for exporting TeXmacs documents as PDF documents
  
## How to install Ghostscript
### Windows
Go to the [Ghostscript official website](https://www.ghostscript.com) to download the installation package and install it to the system C drive according to the default path.

If users didn't install according to the default path, they need to manually add the directory where `gs.exe` is located to the PATH.

### macOS
Use `brew install ghostscript` to install.

### Linux
Use `sudo apt install ghostscript` or other ways to install.
