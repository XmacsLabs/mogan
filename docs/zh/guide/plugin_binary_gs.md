# Ghoscript 二进制插件

- 二进制标识：`gs`
- 插件源代码：
  - [Codeberg](https://codeberg.org/XmacsLabs/mogan/src/branch/branch-1.2/TeXmacs/plugins/binary/progs/binary/gs.scm)
  - [Gitee](https://gitee.com/XmacsLabs/mogan/blob/branch-1.2/TeXmacs/plugins/binary/progs/binary/gs.scm)
  - [Github](https://github.com/XmacsLabs/mogan/blob/branch-1.2/TeXmacs/plugins/binary/progs/binary/gs.scm)
- 功能：
  - 将 PS/PDF 矢量图转换为 PDF 位图，以在墨干中渲染
  - 将 PS 矢量图转换为 PDF 矢量图，以将 TeXmacs 文档导出为 PDF 文档

## 如何安装 Ghostscript

### Windows

前往[Ghostscript 官网](https://www.ghostscript.com)下载安装包并按照默认路径安装到系统 C 盘。

如果用户没有按照默认路径安装，则需要手动将`gs.exe`所在目录添加到 PATH 中。

### macOS

使用`brew install ghostscript`安装。

### Linux

使用`sudo apt install ghostscript`等方式安装。
