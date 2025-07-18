# 在GNU/Linux上开发

## 使用xmake
### 第一步：安装xmake和Qt 6
以Debian及衍生版为例：
```
sudo apt install xmake

sudo apt update
sudo apt install --yes build-essential libfontconfig1-dev  qt6-base-dev libqt6svg6-dev qt6-image-formats-plugins libcurl4-openssl-dev libfreetype-dev libgit2-dev zlib1g-dev libssl-dev libjpeg-turbo8-dev cmake
```

有时候需要保持xrepo是最新的，以保证使用最新的依赖的构建定义
```
xrepo update-repo
```

### 第二步：编译
```
xmake config --yes
xmake build stem
```

如果找不到Qt，那么在config的时候，可以手动指定，比如：
```
xmake config --qt=/usr/lib/`arch`-linux-gnu/qt6/
```
切换到Qt 5，也只需要：
```
xmake config --qt=/usr/lib/`arch`-linux-gnu/qt5/
```

### 第三步：测试
见[如何测试](Test_ZH.md)。

### 第四步：启动墨干
``` bash
xmake run stem
```

## 使用VSCode帮助代码补全
安装VSCode，Clangd以及VSCode的Clangd插件。
之后在mogan文件夹执行
```
xmake project -k compile_commands
```
这个命令会在mogan文件夹下生成一个compile_commands.json文件，Clangd会读取它以理解项目的组织结构，从而避免找不到头文件的错误。
