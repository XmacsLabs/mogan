# PDF图像插件
墨干的渲染器无法支持PDF图像，通常此类PDF图像是PDF矢量图，由第三方软件制作（比如在Octave或者Maxima导出为PDF矢量图）。

墨干在渲染PDF图像时，需要依赖于Ghostscipt将PDF图像转换为PNG图像。故而该插件需要用户手动安装Ghostscript。

## 如何安装Ghostscript
### Windows
前往[Ghostscript官网](https://www.ghostscript.com)下载安装包并按照默认路径安装到系统C盘。

如果用户没有按照默认路径安装，则需要手动将`gs.exe`所在目录添加到PATH中。

### macOS
使用`brew install ghostscript`安装。

### Linux
使用`sudo apt install ghostscript`等方式安装。
