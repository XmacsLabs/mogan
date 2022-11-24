# 开发者文档
## 使用cmake在Windows上开发
我们需要在Windows上使用WSL里面的Ubuntu 20.04或者在Ubuntu 20.04上启动Windows的虚拟机。因为构建是在Ubuntu上面完成，而测试是在Windows上完成。

首先，我们需要完成MXE的下载和相关依赖的安装，可以参考这个文档：
https://texmacs.github.io/notes/docs/build-using-cmake-and-mxe-on-wsl.html

假定你已经在mogan的源代码目录：
```
rm -rf build/
./packages/windows/package.sh
```
安装完成之后，我们需要通过虚拟机和宿主机之间的文件共享，在Windows系统中启动墨干，并做测试。
