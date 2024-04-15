# Python插件
相关教学视频：[零基础墨干06：如何在墨干中学Python语言](https://www.bilibili.com/video/BV1hm421771h/)

## 安装
Python插件依赖于[Python 3二进制插件](plugin_binary_python3.md)，先按照Python 3二进制插件的文档安装Python 3，然后就可以插入Python会话了。

Python插件中的`ps_out`函数可以绘制Postscript矢量图，而Postscript矢量图的渲染和PDF导出依赖于[Ghostscript二进制插件](plugin_binary_gs.md)，如有矢量图的绘制需求，请先按照文档安装Ghostscript。

Python插件支持从Conda创建的Python虚拟环境启动，请先按照[Conda二进制插件](plugin_binary_conda.md)安装Conda并创建相关虚拟环境。

## 如何从Conda虚拟环境启动Python会话
假定你已经安装了Miniconda，并且Conda二进制插件能够正确检测到你安装的Conda。

通过如下命令就可以创建一个名为`plot`的Python虚拟环境：
```
conda create -n plot python matplotlib
```
在plot这个虚拟环境中，我们预装了matplotlib这个Python软件包。

然后，我们就可以通过`插件->会话->Python->conda_plot`基于`plot`这个Python虚拟环境启动Python会话，在这个Python会话中，你可以`import matplotlib`，然后调用matplotlib软件包里面的函数绘图。如果`插件->会话->Python`中没有出现`conda_plot`这个选项，那么你可以重启墨干，或者点击`工具->更新->插件`，再做相关操作。

## 内置文档见：`帮助->插件->Python`
