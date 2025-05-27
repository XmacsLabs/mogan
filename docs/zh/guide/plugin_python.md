# Python 插件

相关教学视频：[零基础墨干 06：如何在墨干中学 Python 语言](https://www.bilibili.com/video/BV1hm421771h/)

## 安装

Python 插件依赖于[Python 3 二进制插件](plugin_binary_python3.md)，先按照 Python 3 二进制插件的文档安装 Python 3，然后就可以插入 Python 会话了。

Python 插件中的`ps_out`函数可以绘制 Postscript 矢量图，而 Postscript 矢量图的渲染和 PDF 导出依赖于[Ghostscript 二进制插件](plugin_binary_gs.md)，如有矢量图的绘制需求，请先按照文档安装 Ghostscript。

Python 插件支持从 Conda 创建的 Python 虚拟环境启动，请先按照[Conda 二进制插件](plugin_binary_conda.md)安装 Conda 并创建相关虚拟环境。

## 如何从 Conda 虚拟环境启动 Python 会话

假定你已经安装了 Miniconda，并且 Conda 二进制插件能够正确检测到你安装的 Conda。

通过如下命令就可以创建一个名为`plot`的 Python 虚拟环境：

```
conda create -n plot python matplotlib
```

在 plot 这个虚拟环境中，我们预装了 matplotlib 这个 Python 软件包。

然后，我们就可以通过`插件->会话->Python->conda_plot`基于`plot`这个 Python 虚拟环境启动 Python 会话，在这个 Python 会话中，你可以`import matplotlib`，然后调用 matplotlib 软件包里面的函数绘图。如果`插件->会话->Python`中没有出现`conda_plot`这个选项，那么你可以重启墨干，或者点击`工具->更新->插件`，再做相关操作。

## 内置文档见：`帮助->插件->Python`
