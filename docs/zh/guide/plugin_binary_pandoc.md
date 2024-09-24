# Pandoc 二进制插件
+ 可执行文件名：pandoc
+ 插件源代码：
  - [Codeberg](https://codeberg.org/XmacsLabs/mogan/src/branch/branch-1.2/TeXmacs/plugins/binary/progs/binary/pandoc.scm)
  - [Gitee](https://gitee.com/XmacsLabs/mogan/blob/branch-1.2/TeXmacs/plugins/binary/progs/binary/pandoc.scm)
  - [Github](https://github.com/XmacsLabs/mogan/blob/branch-1.2/TeXmacs/plugins/binary/progs/binary/pandoc.scm)
- 功能：支持`文档转换->导出到多种格式`
+ 墨干：`>= v1.2.8`

## Pandoc介绍
+ **文件转换**：Pandoc 是一个强大的工具，可以在多种标记格式之间进行转换，包括 Markdown、LaTeX、HTML 等。
+ **Markdown 扩展**：Pandoc 支持多种有用的 Markdown 语法扩展，如文档元数据、脚注、表格、定义列表、上标和下标、删除线、增强的有序列表等。
+ **数学公式**：支持 LaTeX 数学公式，可以通过 MathJax 或 MathML 等多种方法在 HTML 中渲染数学公式。
+ **自动引用和书目**：Pandoc 包含一个强大的系统，可以自动生成引用和书目，支持多种引用格式和书目数据格式。

## 如何安装 Pandoc
### Windows
使用包安装工具：

+ 使用`Chocolatey`安装：
```
choco install pandoc
```
+ 使用`winget`安装：
```
winget install --source winget --exact --id JohnMacFarlane.Pandoc
```

使用安装程序：
下载并运行Pandoc的安装程序，它会自动将Pandoc安装到系统路径中。

安装位置：
通常在 `C:\Program Files\Pandoc\pandoc.exe`。

### macOS
使用`brew install pandoc`安装。

安装位置：
通常在 `/usr/local/bin/pandoc`。

### Linux
在Debian bookworm中，使用如下命令安装：
```
sudo apt install pandoc
```
也可以使用Conda进行安装：
```
conda install -c conda-forge pandoc
```

安装位置：
通常在 `/usr/bin/pandoc` 或 `/usr/local/bin/pandoc`。

### 如何验证是否安装成功？
要验证 Pandoc 是否成功安装，可以按照以下步骤操作：

+ 命令行检查：
  - 打开命令行终端（Windows 的 CMD 或 PowerShell，macOS 的 Terminal，Linux 的 Shell）。
  - 输入 pandoc -v 或 pandoc --version，查看 Pandoc 版本信息。如果显示版本号，则表示安装成功。
+ 转换测试：
  - 创建一个简单的 Markdown 文件（例如 test.md），内容可以是 # Hello, Pandoc!。
  - 在命令行中运行 pandoc test.md -o test.pdf，将 Markdown 文件转换为 PDF。如果生成了 test.pdf 文件，则表示 Pandoc 工作正常。
+ 路径检查：
  - 确认 Pandoc 的安装路径已添加到系统的环境变量中。可以在命令行中输入 echo $PATH（Linux 和 macOS）或 echo %PATH%（Windows），查看 Pandoc 的安装路径是否包含在内。
