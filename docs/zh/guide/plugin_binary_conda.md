# Conda二进制插件
+ 二进制标识：`conda`
+ 插件源代码：
  - [Codeberg](https://codeberg.org/XmacsLabs/mogan/src/branch/branch-1.2/TeXmacs/plugins/binary/progs/binary/conda.scm)
  - [Gitee](https://gitee.com/XmacsLabs/mogan/blob/branch-1.2/TeXmacs/plugins/binary/progs/binary/conda.scm)
  - [Github](https://github.com/XmacsLabs/mogan/blob/branch-1.2/TeXmacs/plugins/binary/progs/binary/conda.scm)
+ 功能：为[Python插件](plugin_python.md)提供Python虚拟环境

## 如何安装Conda
请安装官网安装Miniconda: https://docs.anaconda.com/free/miniconda/

按照默认路径安装的Miniconda是能够直接被识别的：
+ Linux: `$HOME/miniconda3/bin/conda`
+ macOS: `$HOME/miniconda3/bin/conda`
+ Windows: `$USERPROFILE/miniconda3/Scripts/conda.exe`

如果不是默认路径，则需要安装二进制插件的文档做相关配置。
